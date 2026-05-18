package datomlite

import scala.language.dynamics
import scala.quoted.*

/** Read-only view of a pinned db state
  *
  * The bare minimum a `Pull[A]` needs to resolve attributes lazily. `Live` and `Snap` both
  * implement this against a single `State` snapped at pull time so lookups inside one closure are
  * consistent
  */
trait PullCtx {
  def scalarOf(eid: Long, attr: String): Any
  def multiValueOf(eid: Long, attr: String): Set[Any]
  def refEidOf(eid: Long, attr: String): Long
  def multiRefEidsOf(eid: Long, attr: String): Vector[Long]
  def reconstructAt[A](eid: Long, e: Entity[A]): A
  def existsAs[A](eid: Long, e: Entity[A]): Boolean

  /** Reverse-ref lookup
    *
    * Every eid whose `attr` (in `"Class/field"` form) holds the given target eid. Backed by the
    * AVET index, so this is the same hash probe a forward `byEqByName` runs
    */
  def reverseEids(attr: String, eid: Long): Vector[Long]
}

/** Lazy handle to one entity
  *
  * Field access (`p.name`, `p.dept(_.name)`) is rewritten by the `selectDynamic` macro into AVET /
  * EAVT lookups against the bound `PullCtx`. Use `.value` to materialize the full entity, or
  * `.apply(sub)` to drill into a sub-pull
  */
class Pull[A](
    val ctx: PullCtx,
    val eid: Long,
    val entity: Entity[A]
) extends Dynamic {

  /** Materialize the whole entity. Equivalent to `db.byEid[A](eid)` but reuses the pinned ctx. */
  def value: A = ctx.reconstructAt(eid, entity)

  /** Run a sub-selector against this pull. Lets ref descent compose: `e.dept(_.name)` ⇒ `String`.
    */
  def apply[R](f: Pull[A] => R): R = f(this)

  transparent inline def selectDynamic(inline name: String): Any =
    ${ Pull.selectImpl[A]('this, 'name) }

  /** Reverse-ref descent.
    *
    * Every B that points back to me through `f` (a single-field selector on B). The inline lambda
    * must be `_.field` and the field on B must be a ref to A.. both checked at compile time
    *
    * {{{
    *   db.pull[Department](deptEid)(d => d.reverse[Employee](_.dept))               // MultiPull[Employee]
    *   db.pull[Department](deptEid)(d => d.reverse[Employee](_.dept).map(_.name))   // Vector[String]
    * }}}
    */
  inline def reverse[B](inline f: B => Any)(using eb: Entity[B]): MultiPull[B] =
    ${ Pull.reverseImpl[A, B]('this, 'f, 'eb) }
}

/** Card-many ref pull. `.map(sub)` fans `sub` over each pull, `.value` materializes them all. */
class MultiPull[A](val pulls: Vector[Pull[A]]) {
  def map[R](f: Pull[A] => R): Vector[R] = pulls.map(f)
  def value: Vector[A] = pulls.map(_.value)
  def isEmpty: Boolean = pulls.isEmpty
  def size: Int = pulls.size
}

object Pull {

  /** Macro... rewrites `pull.fieldName` based on the field's case-class shape
    *
    *   - scalar `T` ⇒ `ctx.scalarOf(...).asInstanceOf[T]`
    *   - `Set[T]` of opaque value ⇒ `ctx.multiValueOf(...).asInstanceOf[Set[T]]`
    *   - ref `R` (with `Entity[R]` in scope) ⇒ `new Pull[R](ctx, refEid, refEntity)`
    *   - `Set[R]` (multi-ref) ⇒ `new MultiPull[R](pulls)`
    *
    * Unknown field is a compile error. Ref-target Entity is fetched at runtime via
    * `parent.entity.shapes(idx).target()`, so no implicit search at the splice site
    */
  def selectImpl[A: Type](pull: Expr[Pull[A]], name: Expr[String])(using q: Quotes): Expr[Any] = {
    import q.reflect.*
    val fieldName = name.valueOrAbort
    val typeRepr = TypeRepr.of[A]
    val className = typeRepr.typeSymbol.name
    val caseFields = typeRepr.typeSymbol.caseFields
    val fieldSym = caseFields.find(_.name == fieldName) match {
      case Some(s) => s
      case None =>
        val available = caseFields.map(_.name).mkString(", ")
        report.errorAndAbort(s"`$fieldName` is not a field of $className. Available: $available")
    }

    val fieldType = typeRepr.memberType(fieldSym).widen
    val fieldNameExpr = Expr(fieldName)
    val idxExpr = Expr(caseFields.indexWhere(_.name == fieldName))

    def isRefType(t: TypeRepr): Boolean =
      Implicits.search(TypeRepr.of[Entity].appliedTo(List(t))) match {
        case _: ImplicitSearchSuccess => true
        case _                        => false
      }

    fieldType match {
      case AppliedType(tycon, List(elem)) if tycon =:= TypeRepr.of[Set] =>
        if isRefType(elem) then
          elem.asType match {
            case '[t] =>
              '{
                val pullV = $pull
                val attr = pullV.entity.name + "/" + $fieldNameExpr
                val eids = pullV.ctx.multiRefEidsOf(pullV.eid, attr)
                val refEnt = pullV.entity.shapes($idxExpr) match {
                  case FieldShape.MultiRef(target) => target().asInstanceOf[Entity[t]]
                  case other => sys.error(s"expected MultiRef shape, got $other")
                }
                new MultiPull[t](eids.map(eid => new Pull[t](pullV.ctx, eid, refEnt)))
              }
          }
        else
          elem.asType match {
            case '[t] =>
              '{
                val pullV = $pull
                val attr = pullV.entity.name + "/" + $fieldNameExpr
                pullV.ctx.multiValueOf(pullV.eid, attr).asInstanceOf[Set[t]]
              }
          }

      case AppliedType(tycon, List(elem)) if tycon =:= TypeRepr.of[Option] =>
        elem.asType match {
          case '[t] =>
            '{
              val pullV = $pull
              val attr = pullV.entity.name + "/" + $fieldNameExpr
              pullV.ctx.multiValueOf(pullV.eid, attr).headOption.asInstanceOf[Option[t]]
            }
        }

      case _ if isRefType(fieldType) =>
        fieldType.asType match {
          case '[t] =>
            '{
              val pullV = $pull
              val attr = pullV.entity.name + "/" + $fieldNameExpr
              val refEid = pullV.ctx.refEidOf(pullV.eid, attr)
              val refEnt = pullV.entity.shapes($idxExpr) match {
                case FieldShape.Ref(target) => target().asInstanceOf[Entity[t]]
                case other                  => sys.error(s"expected Ref shape, got $other")
              }
              new Pull[t](pullV.ctx, refEid, refEnt)
            }
        }

      case _ =>
        fieldType.asType match {
          case '[t] =>
            '{
              val pullV = $pull
              val attr = pullV.entity.name + "/" + $fieldNameExpr
              pullV.ctx.scalarOf(pullV.eid, attr).asInstanceOf[t]
            }
        }
    }
  }

  /** Macro... validates `f` is a single-field selector `(_.field)` on `B`
    *
    * Checks that the field is a `Ref[A]` or `MultiRef[A]` (i.e. the back-link target equals `A`),
    * and emits the AVET probe via `ctx.reverseEids`. Errors out with the available-fields hint on a
    * typo
    */
  def reverseImpl[A: Type, B: Type](
      pull: Expr[Pull[A]],
      f: Expr[B => Any],
      eb: Expr[Entity[B]]
  )(using q: Quotes): Expr[MultiPull[B]] = {
    import q.reflect.*

    def unwrap(t: Term): Term = t match {
      case Inlined(_, _, body) => unwrap(body)
      case Typed(inner, _)     => unwrap(inner)
      case Block(Nil, expr)    => unwrap(expr)
      case _                   => t
    }

    val aRepr = TypeRepr.of[A]
    val bRepr = TypeRepr.of[B]
    val bClassName = bRepr.typeSymbol.name
    val bFields = bRepr.typeSymbol.caseFields

    val fieldName: String = unwrap(f.asTerm) match {
      case Lambda(List(p), body) =>
        unwrap(body) match {
          case Select(Ident(_), name) => name
          case other =>
            report.errorAndAbort(
              s"reverse[$bClassName] expects a single-field selector like `_.field`, got: ${other.show}"
            )
        }
      case other =>
        report.errorAndAbort(
          s"reverse[$bClassName] expects an inline lambda `(_.field)`, got: ${other.show}"
        )
    }

    val fieldSym = bFields.find(_.name == fieldName).getOrElse {
      val available = bFields.map(_.name).mkString(", ")
      report.errorAndAbort(
        s"`$fieldName` is not a field of $bClassName. Available: $available"
      )
    }

    val fieldType = bRepr.memberType(fieldSym).widen

    val refTarget: TypeRepr = fieldType match {
      case AppliedType(tycon, List(elem)) if tycon =:= TypeRepr.of[Set] => elem
      case t                                                            => t
    }

    if !(refTarget =:= aRepr) then
      report.errorAndAbort(
        s"`$bClassName.$fieldName` does not target ${aRepr.show}; reverse[$bClassName] needs a ref" +
          s" field whose target is the surrounding pull's entity"
      )

    val fieldNameExpr = Expr(fieldName)
    '{
      val pullV = $pull
      val ebV = $eb
      val attr = ebV.name + "/" + $fieldNameExpr
      val eids = pullV.ctx.reverseEids(attr, pullV.eid)
      new MultiPull[B](eids.map(eid => new Pull[B](pullV.ctx, eid, ebV)))
    }
  }
}
