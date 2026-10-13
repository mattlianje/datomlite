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
    *   - scalar `T` ⇒ `T`, decoded through the field's codec
    *   - `Set[T]` / `Option[T]` of values ⇒ `Set[T]` / `Option[T]`, decoded per element
    *   - ref `R` (with `Entity[R]` in scope) ⇒ `Pull[R]`
    *   - `Set[R]` ⇒ `MultiPull[R]`, `Option[R]` ⇒ `Option[Pull[R]]`
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
    val idxExpr = Expr(caseFields.indexWhere(_.name == fieldName))

    def isRefType(t: TypeRepr): Boolean =
      Implicits.search(TypeRepr.of[Entity].appliedTo(List(t))) match {
        case _: ImplicitSearchSuccess => true
        case _                        => false
      }

    fieldType match {
      case AppliedType(tycon, List(elem)) if tycon =:= TypeRepr.of[Set] =>
        elem.asType match {
          case '[t] =>
            if isRefType(elem) then '{ Pull.multiRef[t]($pull, $idxExpr) }
            else '{ Pull.multi($pull, $idxExpr).asInstanceOf[Set[t]] }
        }

      case AppliedType(tycon, List(elem)) if tycon =:= TypeRepr.of[Option] =>
        elem.asType match {
          case '[t] =>
            if isRefType(elem) then '{ Pull.optionalRef[t]($pull, $idxExpr) }
            else '{ Pull.optional($pull, $idxExpr).asInstanceOf[Option[t]] }
        }

      case _ =>
        fieldType.asType match {
          case '[t] =>
            if isRefType(fieldType) then '{ Pull.ref[t]($pull, $idxExpr) }
            else '{ Pull.scalar($pull, $idxExpr).asInstanceOf[t] }
        }
    }
  }

  /* Runtime side of `selectDynamic`. `idx` is the field's position in the case class */

  private[datomlite] def scalar(p: Pull[?], idx: Int): Any = {
    val e = p.entity
    val field = e.fields(idx)
    p.ctx.multiValueOf(p.eid, attrOf(p, idx)).headOption match {
      case Some(v) => e.shapes(idx).codec.decode(v)
      case None =>
        e.defaults.get(field).fold(throw new MissingField(e.name, field, Some(p.eid)))(_())
    }
  }

  private[datomlite] def multi(p: Pull[?], idx: Int): Set[Any] = {
    val codec = p.entity.shapes(idx).codec
    p.ctx.multiValueOf(p.eid, attrOf(p, idx)).map(codec.decode)
  }

  private[datomlite] def optional(p: Pull[?], idx: Int): Option[Any] = {
    val codec = p.entity.shapes(idx).codec
    p.ctx.multiValueOf(p.eid, attrOf(p, idx)).headOption.map(codec.decode)
  }

  private[datomlite] def ref[R](p: Pull[?], idx: Int): Pull[R] =
    new Pull[R](p.ctx, p.ctx.refEidOf(p.eid, attrOf(p, idx)), targetOf[R](p, idx))

  private[datomlite] def multiRef[R](p: Pull[?], idx: Int): MultiPull[R] = {
    val target = targetOf[R](p, idx)
    new MultiPull[R](p.ctx.multiRefEidsOf(p.eid, attrOf(p, idx)).map(new Pull[R](p.ctx, _, target)))
  }

  private[datomlite] def optionalRef[R](p: Pull[?], idx: Int): Option[Pull[R]] =
    p.ctx.multiRefEidsOf(p.eid, attrOf(p, idx)).headOption
      .map(new Pull[R](p.ctx, _, targetOf[R](p, idx)))

  private def attrOf(p: Pull[?], idx: Int): String = s"${p.entity.name}/${p.entity.fields(idx)}"

  private def targetOf[R](p: Pull[?], idx: Int): Entity[R] =
    (p.entity.shapes(idx) match {
      case FieldShape.Ref(target)         => target()
      case FieldShape.MultiRef(target)    => target()
      case FieldShape.OptionalRef(target) => target()
      case other                          => sys.error(s"expected a ref shape, got $other")
    }).asInstanceOf[Entity[R]]

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
      case AppliedType(tycon, List(elem))
          if tycon =:= TypeRepr.of[Set] || tycon =:= TypeRepr.of[Option] =>
        elem
      case t => t
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
