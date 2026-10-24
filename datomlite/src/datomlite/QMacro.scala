package datomlite

import scala.quoted.*

object QMacro {

  /** Try to compile `pred` into an indexed AVET probe returning `Vector[A]`...
    *
    * Knows four exact leaf shapes:
    *
    *   1. `_.field == const`.. value-eq, point probe on AVET
    *   1. `_.refField == refValue`.. ref-eq, resolve `refValue` to its eid then probe
    *   1. `_.refField.leaf == const`.. ref-walk, probe leaf attr, then probe ref attr per parent
    *      eid
    *   1. `_.set.contains(x)` / `_.opt.contains(x)`.. membership on a `Set` or `Option` field,
    *      values or refs
    *
    * and two range shapes on a numeric or `String` field that slice the sorted AVET:
    *
    *   1. `_.field > const` (and `>=`, `<`, `<=`, either way round)
    *   1. `_.field.startsWith(const)`
    *
    * A `&&` of leaves probes by its first exact leaf or else by the ranges on its tightest field...
    * then filters those rows with the whole predicate. `None` if nothing is indexable and callers
    * fall back to a scan
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

    /** Element type of a `Set[T]` / `Option[T]` field */
    def elemOf(field: String, coll: TypeRepr): Option[TypeRepr] =
      memberType(typeRepr, field).collect {
        case ft @ AppliedType(_, List(elem)) if ft <:< coll => elem
      }

    /** Types stored as is. A literal of a neighbouring numeric type still probes the right AVET
      * bucket (`_.age == 30` on a `Long` field)
      */
    val storedAsIs = List(
      TypeRepr.of[Int],
      TypeRepr.of[Long],
      TypeRepr.of[Double],
      TypeRepr.of[String],
      TypeRepr.of[Boolean]
    )

    /** A probe encodes the literal with the field's codec so it has to really be of the field's
      * type. Anything else falls back to a scan (`==` is untyped)
      */
    def probeable(value: Term, fieldType: TypeRepr): Boolean = {
      val vt = value.tpe.widen
      vt <:< fieldType || (storedAsIs.exists(fieldType =:= _) && storedAsIs.exists(vt <:< _))
    }

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
            val parentEids =
              $db.internal.eidsByEqByName[t](${ Expr(leafField) }, $leafValue)(using $parentEnt)
            parentEids.iterator
              .flatMap(pid => $db.internal.byEqByName[A](${ Expr(parentField) }, pid)(using $e).run)
              .toVector
              .distinct
          }
      }
    }

    /** Probe code for `_.set.contains(x)` / `_.set(x)` / `_.opt.contains(x)`
      *
      * Same shape as value-eq because card-many fans out per element on the AVET
      */
    def emitSetContains(field: String, valueTerm: Term): Expr[Vector[A]] =
      emitValueEq(field, valueTerm)

    /** One leaf as an exact probe. Its rows are exactly the ones the leaf accepts */
    def exactLeaf(leaf: Term, param: Symbol): Option[Expr[Vector[A]]] =
      unwrap(leaf) match {
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
                  case Some(ft) if probeable(valueSide, ft) =>
                    Some(emitValueEq(field, valueSide))
                  case _ => None
                }
              case Some(List(parent, leaf)) if caseFields.contains(parent) =>
                memberType(typeRepr, parent) match {
                  case Some(pt) if isRefType(pt) =>
                    memberType(pt, leaf) match {
                      case Some(lt)
                          if !isRefType(lt) && !isSetType(lt) && probeable(valueSide, lt) =>
                        Some(emitRefWalk(parent, pt, leaf, valueSide))
                      case _ => None
                    }
                  case _ => None
                }
              case _ => None
            }

        case Apply(Select(maybeColl, method @ ("contains" | "apply")), List(arg)) =>
          // Set#contains, Set#apply (Function1), or Option#contains. AVET stores bare values for
          // both card-many sets and Option fields, so probing by the arg matches the same rows as
          // the predicate would. Ref elements probe by the target's eid
          unwrap(maybeColl) match {
            case Select(qual, name) if qual.symbol == param && !referencesParam(arg, param) =>
              val elem = elemOf(name, TypeRepr.of[Set[?]]).orElse(
                if method == "contains" then elemOf(name, TypeRepr.of[Option[Any]]) else None
              )
              elem match {
                case Some(et) if isRefType(et) && arg.tpe.widen <:< et =>
                  Some(emitRefEq(name, arg, et))
                case Some(et) if probeable(arg, et) => Some(emitSetContains(name, arg))
                case _                              => None
              }
            case _ => None
          }
        case _ => None
      }

    /** Field types that order the same in Scala as in stored form */
    val ordered = List(
      TypeRepr.of[Int],
      TypeRepr.of[Long],
      TypeRepr.of[Double],
      TypeRepr.of[BigInt],
      TypeRepr.of[BigDecimal]
    )

    /** Opaque over an ordered stored form. Only compared against their own kind */
    val opaqueOrdered = List(TypeRepr.of[Time], TypeRepr.of[Date])

    def isString(t: TypeRepr): Boolean = t =:= TypeRepr.of[String]

    def rangeable(field: String, value: Term): Boolean = {
      val vt = value.tpe.widen
      memberType(typeRepr, field).exists { ft =>
        if isString(ft) || opaqueOrdered.exists(ft =:= _) then vt <:< ft
        else ordered.exists(ft =:= _) && ordered.exists(vt <:< _)
      }
    }

    /** `_.field`, even behind an implicit conversion like `augmentString(_.field)` */
    def fieldOf(t: Term, param: Symbol): Option[String] = unwrap(t) match {
      case Apply(fn, List(arg)) if fn.symbol.flags.is(Flags.Implicit) => fieldOf(arg, param)
      case Apply(Apply(fn, List(arg)), _) if fn.symbol.flags.is(Flags.Implicit) =>
        fieldOf(arg, param)
      case other =>
        selectChain(other, param).collect { case List(f) if caseFields.contains(f) => f }
    }

    /** What one leaf says about one field. Only one of the three is set */
    final case class Range(field: String, lo: Option[Term], hi: Option[Term], prefix: Option[Term])

    def rangeLeaf(leaf: Term, param: Symbol): Option[Range] = {
      def bound(field: String, op: String, value: Term): Range =
        if op.startsWith(">") then Range(field, Some(value), None, None)
        else Range(field, None, Some(value), None)
      def flip(op: String): String = op.replace('<', '?').replace('>', '<').replace('?', '>')
      def compare(lhs: Term, op: String, rhs: Term): Option[Range] =
        (fieldOf(lhs, param), fieldOf(rhs, param)) match {
          case (Some(f), None) if !referencesParam(rhs, param) && rangeable(f, rhs) =>
            Some(bound(f, op, rhs))
          case (None, Some(f)) if !referencesParam(lhs, param) && rangeable(f, lhs) =>
            Some(bound(f, flip(op), lhs))
          case _ => None
        }
      val ops = Set(">", ">=", "<", "<=")
      unwrap(leaf) match {
        case Apply(Select(lhs, op), List(rhs)) if ops(op) => compare(lhs, op, rhs)
        /* `Time` / `Date` compare through extension methods */
        case Apply(Apply(fn, List(lhs)), List(rhs))
            if ops(fn.symbol.name) && fn.symbol.flags.is(Flags.ExtensionMethod) =>
          compare(lhs, fn.symbol.name, rhs)
        case Apply(Select(lhs, "startsWith"), List(arg))
            if !referencesParam(arg, param) && arg.tpe.widen <:< TypeRepr.of[String] =>
          fieldOf(lhs, param)
            .filter(f => memberType(typeRepr, f).exists(isString))
            .map(Range(_, None, None, Some(arg)))
        case _ => None
      }
    }

    /** Slice on the field the leaves bound tightest... a prefix, then both sides, then one */
    def emitRange(ranges: List[Range]): Option[Expr[Vector[A]]] = {
      def score(rs: List[Range]): Int =
        (if rs.exists(_.prefix.isDefined) then 2 else 0) +
          (if rs.exists(_.lo.isDefined) then 1 else 0) +
          (if rs.exists(_.hi.isDefined) then 1 else 0)
      ranges.groupBy(_.field).toList.sortBy((_, rs) => -score(rs)).headOption.map { (field, rs) =>
        def side(get: Range => Option[Term]): Expr[Option[Any]] =
          rs.flatMap(get).headOption.fold('{ None })(v => '{ Some(${ v.asExprOf[Any] }) })
        val prefix: Expr[Option[String]] =
          rs.flatMap(_.prefix).headOption.fold('{ None })(v => '{ Some(${ v.asExprOf[String] }) })
        val (lo, hi) = (side(_.lo), side(_.hi))
        '{ $db.internal.byRangeByName[A](${ Expr(field) }, $lo, $hi, $prefix)(using $e).run }
      }
    }

    def conjuncts(t: Term): List[Term] = unwrap(t) match {
      case Apply(Select(l, "&&"), List(r)) => conjuncts(l) ++ conjuncts(r)
      case other                           => List(other)
    }

    def filtered(rows: Expr[Vector[A]]): Expr[Vector[A]] = '{ $rows.filter($pred) }

    unwrap(pred.asTerm) match {
      case Lambda(List(p), body) =>
        val param = p.symbol
        conjuncts(body) match {
          case List(leaf) =>
            exactLeaf(leaf, param)
              .orElse(rangeLeaf(leaf, param).flatMap(r => emitRange(List(r))).map(filtered))
          case leaves =>
            leaves.iterator.flatMap(exactLeaf(_, param)).nextOption()
              .orElse(emitRange(leaves.flatMap(rangeLeaf(_, param))))
              .map(filtered)
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
    * The match runs as a tx fn against the state the commit applies to. So a row written between
    * finding and retracting can't slip through. Goes via `whereImpl` so AVET-eligible predicates
    * skip the scan
    */
  def retractWhereImpl[A: Type](
      db: Expr[Db],
      pred: Expr[A => Boolean],
      e: Expr[Entity[A]]
  )(using q: Quotes): Expr[TxReport] =
    '{
      $db.transact(Tx.Fn { (s: Db) =>
        val matches = ${ whereImpl[A]('s, pred, e) }.run
        Tx.Retract(matches.map(a => Datum(a, $e.asInstanceOf[Entity[Any]])))
      })
    }

  /** Compile `db.upsertWhere[A](pred)(f)` into an atomic find-then-upsert
    *
    * Finds inside the commit like `retractWhereImpl` so concurrent read-modify-writes queue up
    * instead of losing updates. Runs `f` on each match and upserts the results as one step
    */
  def upsertWhereImpl[A: Type](
      db: Expr[Db],
      pred: Expr[A => Boolean],
      f: Expr[A => A],
      e: Expr[Entity[A]]
  )(using q: Quotes): Expr[TxReport] =
    '{
      $db.transact(Tx.Fn { (s: Db) =>
        val matches = ${ whereImpl[A]('s, pred, e) }.run.map($f)
        Tx.Upsert(matches.map(a => Datum(a, $e.asInstanceOf[Entity[Any]])))
      })
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
