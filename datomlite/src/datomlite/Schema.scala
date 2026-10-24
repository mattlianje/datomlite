package datomlite

import scala.quoted.*

/** Thrown on read when a stored entity or value has nothing for a required field
  *
  * Usually the field was added after the entity was written. Give it a default (`age: Int = 0`) or
  * make it an `Option` and older rows read fine
  */
final class MissingField(
    val entityName: String,
    val field: String,
    val eid: Option[Long]
) extends RuntimeException(
      s"$entityName${eid.fold("")(e => s" #$e")} has no value for required field `$field`; " +
        s"give it a default (`$field: T = ...`) or make it an Option"
    )

private[datomlite] object Schema {

  /** Stored form of `field`'s default, if it's a required value field with one
    *
    * Entities written before the field existed have no datom for it... reads, matches and queries
    * treat them as holding this. Option, Set and ref fields are just empty and never fall back
    */
  def fallback(e: Entity[?], field: String): Option[Any] =
    e.shapes(e.fields.indexOf(field)) match {
      case shape: FieldShape.Value => e.defaults.get(field).map(d => shape.codec.encode(d()))
      case _                       => None
    }
}

/** Case-class defaults by field name, grabbed at derivation time
  *
  * Each one is a thunk so a default like `Time.now()` runs when used, same as in the constructor
  */
object Defaults {
  inline def of[A]: Map[String, () => Any] = ${ ofImpl[A] }

  private def ofImpl[A: Type](using Quotes): Expr[Map[String, () => Any]] = {
    import quotes.reflect.*
    val tpe = TypeRepr.of[A]
    val sym = tpe.typeSymbol
    val companion = sym.companionModule
    val entries: List[Expr[(String, () => Any)]] =
      if companion == Symbol.noSymbol then Nil
      else
        sym.caseFields.zipWithIndex.flatMap { (field, i) =>
          companion.methodMember(s"$$lessinit$$greater$$default$$${i + 1}").headOption.map {
            method =>
              val sel = Ref(companion).select(method)
              val default = if tpe.typeArgs.isEmpty then sel else sel.appliedToTypes(tpe.typeArgs)
              '{ ${ Expr(field.name) } -> (() => ${ default.asExprOf[Any] }) }
          }
        }
    '{ Map[String, () => Any](${ Varargs(entries) }*) }
  }
}
