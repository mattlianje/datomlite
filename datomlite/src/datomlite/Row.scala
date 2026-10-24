package datomlite

import scala.language.dynamics
import scala.quoted.*

/** A typed handle bound to an entity type.
  *
  * The closure params from `db.query[Employee] { e => ... }` are `Row[Employee]` values; field
  * accessors (`e.email`, `e.dept`) are statically typed projections
  */
class Row[A] private[datomlite] (
    private[datomlite] val binding: String,
    private[datomlite] val entity: Entity[A]
) extends Term.RowRef[A]
    with Dynamic {
  transparent inline def selectDynamic(inline name: String): Any =
    ${ Row.selectImpl[A]('this, 'name) }
}

object Row {
  /* process wide so two thread racing on db.query don't hand out the same binding */
  private val counter = new java.util.concurrent.atomic.AtomicLong(0)

  private[datomlite] def freshBinding(prefix: String): String =
    s"${prefix.headOption.map(_.toLower).getOrElse('r')}${counter.incrementAndGet()}"

  /** `Term.Field` for one of the row's fields, with its codec and default */
  private[datomlite] def field[A, T](row: Row[A], name: String): Term.Field[T] = {
    val e = row.entity
    val shape = e.shapes(e.fields.indexOf(name))
    Term.Field[T](row.binding, s"${e.name}/$name", shape.codec, Schema.fallback(e, name))
  }

  /** Macro target for `row.fieldName`.
    *
    * Rewrites the access into `Row.field[A, FieldType](row, "fieldName")`. Validates the field name
    * against `A`'s case-class shape; an unknown field is a compile error
    */
  def selectImpl[A: Type](row: Expr[Row[A]], name: Expr[String])(using Quotes): Expr[Any] = {
    import quotes.reflect.*
    val fieldName = name.valueOrAbort
    val typeRepr = TypeRepr.of[A]
    val className = typeRepr.typeSymbol.name
    typeRepr.typeSymbol.caseFields.find(_.name == fieldName) match {
      case None =>
        val available = typeRepr.typeSymbol.caseFields.map(_.name).mkString(", ")
        report.errorAndAbort(
          s"`$fieldName` is not a field of $className. Available: $available"
        )
      case Some(sym) =>
        val fieldType = typeRepr.memberType(sym).widen
        val fieldNameExpr = Expr(fieldName)
        /* Basically matches datalog "match where attribute is present" semantics */
        val effectiveType: TypeRepr = fieldType match {
          case AppliedType(tycon, List(elem)) if tycon =:= TypeRepr.of[Option] => elem
          case _                                                               => fieldType
        }
        effectiveType.asType match {
          case '[t] =>
            '{ datomlite.Row.field[A, t]($row, $fieldNameExpr) }
        }
    }
  }
}
