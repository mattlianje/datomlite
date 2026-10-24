package datomlite

import scala.quoted.*

/** A rewrite of stored datoms as they load
  *
  * Most schema changes need none. A field added with a default (or as an `Option`) reads as that
  * default on older entities. Removed fields are ignored and a stored `Int` still reads as a `Long`
  * / `Double` / `BigInt`. Migrations cover the rest.. renames and clearing out old fields
  *
  * The log is never rewritten. Migrations run in order on every load so keep them as long as the
  * old datoms are around
  *
  * {{{
  *   val storage = FileStorage(path).migrate(
  *     Migration.renameEntity[Person]("User"),
  *     Migration.renameField[Person](_.name, from = "fullName")
  *   )
  *   val db = Db(storage)
  * }}}
  */
final class Migration private (private val step: Datom => Option[Datom]) {

  /** Apply to one datom. `None` drops it */
  def apply(d: Datom): Option[Datom] = step(d)

  def andThen(next: Migration): Migration = new Migration(d => step(d).flatMap(next.step))
}

object Migration {

  /** Any per-datom rewrite. `None` drops it */
  def apply(f: Datom => Option[Datom]): Migration = new Migration(f)

  /** Datoms stored under the old entity name `from` load as `A`'s
    *
    * Put it before any `renameField` on the same entity since that one matches the new name
    */
  def renameEntity[A](from: String)(using e: Entity[A]): Migration = {
    val prefix = s"$from/"
    Migration { d =>
      Some(if d.a.startsWith(prefix) then d.copy(a = e.name + "/" + d.a.drop(prefix.length)) else d)
    }
  }

  /** Datoms stored under `A`'s old field name `from` load as `to` (e.g. `_.name`) */
  inline def renameField[A](inline to: A => Any, from: String)(using e: Entity[A]): Migration =
    renameFieldNamed[A](fieldName[A](to), from)

  /** Drop every datom of `A`'s field `name`, usually one you removed from the case class
    *
    * Reads already ignore removed fields. This just keeps them out of `log`, `pretty` and `history`
    */
  def removeField[A](name: String)(using e: Entity[A]): Migration = {
    val attr = s"${e.name}/$name"
    Migration(d => if d.a == attr then None else Some(d))
  }

  private[datomlite] def renameFieldNamed[A](to: String, from: String)(using
      e: Entity[A]
  ): Migration = {
    val old = s"${e.name}/$from"
    val renamed = s"${e.name}/$to"
    Migration(d => Some(if d.a == old then d.copy(a = renamed) else d))
  }

  /** Field name of a `_.field` selector, checked at compile time */
  inline def fieldName[A](inline f: A => Any): String = ${ fieldNameImpl[A]('f) }

  private def fieldNameImpl[A: Type](f: Expr[A => Any])(using Quotes): Expr[String] = {
    import quotes.reflect.*
    def unwrap(t: Term): Term = t match {
      case Inlined(_, _, body) => unwrap(body)
      case Typed(inner, _)     => unwrap(inner)
      case Block(Nil, expr)    => unwrap(expr)
      case _                   => t
    }
    val fields = TypeRepr.of[A].typeSymbol.caseFields.map(_.name)
    unwrap(f.asTerm) match {
      case Lambda(List(p), body) =>
        unwrap(body) match {
          case Select(Ident(_), name) if fields.contains(name) => Expr(name)
          case other =>
            report.errorAndAbort(
              s"expected a field selector like `_.field` on ${TypeRepr.of[A].show}, got: ${other.show}"
            )
        }
      case other =>
        report.errorAndAbort(s"expected an inline lambda `(_.field)`, got: ${other.show}")
    }
  }
}
