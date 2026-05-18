package datomlite

import scala.quoted.*

object QMacro {

  /** Try to compile `pred` into an indexed AVET probe returning `Vector[A]`...
    *
    * Recognises four shapes, tried in order:
    *
    *   1. `_.field == const`.. value-eq, hash probe on AVET
    *   1. `_.refField == refValue`.. ref-eq, resolve `refValue` to its eid then probe
    *   1. `_.refField.leaf == const`.. ref-walk, probe leaf attr, then probe ref attr per parent
    *      eid
    *   1. `_.set.contains(x)`.. card-many membership on opaque `Set` values
    *
    * Returns `None` if no shape matches; callers fall back to a scan
    */
  private def tryProbeFind[A: Type](
      db: Expr[Db],
      pred: Expr[A => Boolean],
      e: Expr[Entity[A]]
  )(using q: Quotes): Option[Expr[Vector[A]]] = {
    import q.reflect.*

    val typeRepr = TypeRepr.of[A]
    val caseFields = typeRepr.typeSymbol.caseFields.map(_.name).toSet

    def memberType(owner: TypeRepr, field: String): Option[TypeRepr] =
      owner.typeSymbol.caseFields.find(_.name == field).map(owner.memberType)

    def isRefType(t: TypeRepr): Boolean =
      Implicits.search(TypeRepr.of[Entity].appliedTo(List(t))) match {
        case _: ImplicitSearchSuccess => true
        case _                        => false
      }

    def isSetType(t: TypeRepr): Boolean = t <:< TypeRepr.of[Set[?]]

    def isOpaqueSetField(field: String): Boolean =
      memberType(typeRepr, field).exists { ft =>
        isSetType(ft) && (ft match {
          case AppliedType(_, List(elem)) => !isRefType(elem)
          case _                          => false
        })
      }

    def isOptionField(field: String): Boolean =
      memberType(typeRepr, field).exists(ft => ft <:< TypeRepr.of[Option[Any]])

    def unwrap(t: Term): Term = t match {
      case Inlined(_, _, body) => unwrap(body)
      case Typed(inner, _)     => unwrap(inner)
      case Block(Nil, expr)    => unwrap(expr)
      case _                   => t
    }

    def referencesParam(t: Term, param: Symbol): Boolean = {
      var found = false
      val acc = new TreeTraverser {
        override def traverseTree(tree: Tree)(owner: Symbol): Unit =
          if !found then
            tree match {
              case i: Ident if i.symbol == param => found = true
              case _                             => super.traverseTree(tree)(owner)
            }
      }
      acc.traverseTree(t)(Symbol.spliceOwner)
      found
    }

    /** Returns the chain `[f1, f2, ...]` for `_.f1.f2....`, or None if `t` doesn't terminate at the
      * lambda parameter
      */
    def selectChain(t: Term, param: Symbol): Option[List[String]] = unwrap(t) match {
      case i: Ident if i.symbol == param =>
        Some(Nil)
      case Select(qual, name) =>
        selectChain(qual, param).map(_ :+ name)
      case _ =>
        None
    }

    /** Probe code for a value-eq on the top-level entity A. */
    def emitValueEq(field: String, valueTerm: Term): Expr[Vector[A]] = {
      val value = valueTerm.asExprOf[Any]
      '{ $db.internal.byEqByName[A](${ Expr(field) }, $value)(using $e).run }
    }

    /** Probe code for `_.refField == refValue`.
      *
      * Resolve `refValue` to its eid via the ref's Entity, then probe A by that eid value
      */
    def emitRefEq(field: String, refTerm: Term, refType: TypeRepr): Expr[Vector[A]] =
      refType.asType match {
        case '[r] =>
          val refExpr = refTerm.asExprOf[r]
          val refEntE = Expr.summon[Entity[r]].getOrElse(
            report.errorAndAbort(
              s"ref-equality on `_.$field` requires `Entity[${refType.show}]` in scope"
            )
          )
          '{
            $db.internal.eidOf[r]($refExpr)(using $refEntE) match {
              case Some(eid) => $db.internal.byEqByName[A](${ Expr(field) }, eid)(using $e).run
              case None      => Vector.empty[A]
            }
          }
      }

    /** Probe code for `_.parent.leaf == const`
      *
      * Probe `Parent/leaf` for the const, then for each parent eid, probe `A/parent` for that eid
      * value
      */
    def emitRefWalk(
        parentField: String,
        parentType: TypeRepr,
        leafField: String,
        leafValueTerm: Term
    ): Expr[Vector[A]] = {
      val leafValue = leafValueTerm.asExprOf[Any]
      parentType.asType match {
        case '[t] =>
          val parentEnt = Expr.summon[Entity[t]].getOrElse(
            report.errorAndAbort(
              s"ref-walk on `_.$parentField.$leafField` requires `Entity[${parentType.show}]` in scope"
            )
          )
          '{
            val leafAttr = $parentEnt.name + "/" + ${ Expr(leafField) }
            val parentEids = $db.internal.eidsByEq(leafAttr, $leafValue)
            parentEids.iterator
              .flatMap(pid => $db.internal.byEqByName[A](${ Expr(parentField) }, pid)(using $e).run)
              .toVector
              .distinct
          }
      }
    }

    /** Probe code for `_.opaqueSet.contains(x)` / `_.opaqueSet(x)`
      *
      * Same shape as value-eq because card-many fans out per element on the AVET
      */
    def emitSetContains(field: String, valueTerm: Term): Expr[Vector[A]] =
      emitValueEq(field, valueTerm)

    unwrap(pred.asTerm) match {
      case Lambda(List(p), body) =>
        val param = p.symbol
        unwrap(body) match {
          case Apply(Select(lhs, "==" | "equals"), List(rhs)) =>
            // Try chain on lhs; if the rhs references the param, swap.
            val (chainSide, valueSide) =
              if !referencesParam(rhs, param) then (lhs, rhs)
              else if !referencesParam(lhs, param) then (rhs, lhs)
              else (lhs, rhs) // both reference param: bail to scan

            if referencesParam(valueSide, param) then None
            else
              selectChain(chainSide, param) match {
                case Some(List(field)) if caseFields.contains(field) =>
                  memberType(typeRepr, field) match {
                    case Some(ft) if isRefType(ft) =>
                      Some(emitRefEq(field, valueSide, ft))
                    case Some(ft) if ft <:< TypeRepr.of[Option[Any]] =>
                      // _.opt == Some(x) / _.opt == None: AVET stores the bare value, not the
                      // wrapper, so we can't probe with the Option literal. Fall back to scan;
                      // Scala's Option equality matches correctly there.
                      None
                    case Some(_) =>
                      Some(emitValueEq(field, valueSide))
                    case None => None
                  }
                case Some(List(parent, leaf)) if caseFields.contains(parent) =>
                  memberType(typeRepr, parent) match {
                    case Some(pt) if isRefType(pt) =>
                      memberType(pt, leaf) match {
                        case Some(lt) if !isRefType(lt) && !isSetType(lt) =>
                          Some(emitRefWalk(parent, pt, leaf, valueSide))
                        case _ => None
                      }
                    case _ => None
                  }
                case _ => None
              }

          case Apply(Select(maybeColl, method @ ("contains" | "apply")), List(arg)) =>
            // Set#contains, Set#apply (Function1), or Option#contains. AVET stores bare values for
            // both card-many opaque sets and Option fields, so probing by the arg matches the same
            // rows as the predicate would.
            unwrap(maybeColl) match {
              case Select(qual, name) if qual.symbol == param && !referencesParam(arg, param) =>
                if isOpaqueSetField(name) then Some(emitSetContains(name, arg))
                else if isOptionField(name) && method == "contains" then
                  Some(emitValueEq(name, arg))
                else None
              case _ => None
            }
          case _ => None
        }
      case _ => None
    }
  }

  /** Compile `db.where[A](pred)` into a `Query[A]`
    *
    * AVET-routable predicates probe the index and wrap the result in a fresh Query; the rest fall
    * back to a scan over `db.where[A]`
    */
  def whereImpl[A: Type](
      db: Expr[Db],
      pred: Expr[A => Boolean],
      e: Expr[Entity[A]]
  )(using q: Quotes): Expr[Query[A]] =
    tryProbeFind[A](db, pred, e) match {
      case Some(vec) => '{ Query[A]($db, () => ${ vec }.iterator) }
      case None      => '{ $db.where[A](using $e).where($pred) }
    }

  /** Compile `db.retractWhere[A](pred)` into a transact of all matches
    *
    * Routes through `whereImpl` so AVET-eligible predicates skip the scan
    */
  def retractWhereImpl[A: Type](
      db: Expr[Db],
      pred: Expr[A => Boolean],
      e: Expr[Entity[A]]
  )(using q: Quotes): Expr[TxReport] = {
    val queryExpr = whereImpl[A](db, pred, e)
    '{
      val matches = ${ queryExpr }.run
      val datums = matches.map(a => Datum(a, $e.asInstanceOf[Entity[Any]]))
      $db.transact(Tx.Retract(datums))
    }
  }

  /** Compile `db.upsertWhere[A](pred)(f)` into a find-then-upsert
    *
    * Routes through `whereImpl` so AVET-eligible predicates skip the scan, applies `f` to each
    * match, and transacts the result as a single `Tx.Upsert`
    */
  def upsertWhereImpl[A: Type](
      db: Expr[Db],
      pred: Expr[A => Boolean],
      f: Expr[A => A],
      e: Expr[Entity[A]]
  )(using q: Quotes): Expr[TxReport] = {
    val queryExpr = whereImpl[A](db, pred, e)
    '{
      val matches = ${ queryExpr }.run.map($f)
      val datums = matches.map(a => Datum(a, $e.asInstanceOf[Entity[Any]]))
      $db.transact(Tx.Upsert(datums))
    }
  }

  /** Compile `db.exists[A](pred)` into a short-circuit existence check
    *
    * Routes through `whereImpl` so AVET-eligible predicates probe the index instead of scanning
    */
  def existsImpl[A: Type](
      db: Expr[Db],
      pred: Expr[A => Boolean],
      e: Expr[Entity[A]]
  )(using q: Quotes): Expr[Boolean] = {
    val queryExpr = whereImpl[A](db, pred, e)
    '{ ${ queryExpr }.exists }
  }

  /** Compile `db.count[A](pred)` into a `.count` over the matching query. */
  def countImpl[A: Type](
      db: Expr[Db],
      pred: Expr[A => Boolean],
      e: Expr[Entity[A]]
  )(using q: Quotes): Expr[Long] = {
    val queryExpr = whereImpl[A](db, pred, e)
    '{ ${ queryExpr }.count }
  }
}
