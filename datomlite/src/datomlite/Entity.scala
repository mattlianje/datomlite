package datomlite

import scala.compiletime.{constValue, constValueTuple, erasedValue, summonFrom, summonInline}
import scala.deriving.Mirror
import scala.quoted.*
import scala.reflect.ClassTag

/** Field annotation marking a case class field as the entity key
  *
  * Equivalent to chaining `.key(_.field)` after `Entity.derived`. Read at derivation time..
  * composes with the chained builder so `Entity.derived[A].unique(...)` still works for things
  * annotations can't express (composite/computed keys)
  */
final class key extends scala.annotation.StaticAnnotation

/** Field annotation marking a case class field as a unique attribute
  *
  * Equivalent to chaining `.unique(_.field)` after `Entity.derived`. Multiple `@unique` fields are
  * allowed
  */
final class unique extends scala.annotation.StaticAnnotation

trait Entity[A] {
  def name: String
  def clazz: Class[A]
  def fields: List[String]
  def shapes: List[FieldShape]
  def decompose(a: A): Vector[(String, Any)]
  def reconstruct(attrs: Map[String, Any]): A
  def keyOf: Option[A => Any]
  def uniques: List[A => Any]
  def withKey(f: A => Any): Entity[A]
  def unique(f: A => Any): Entity[A]
  def key(f: A => Any): Entity[A] = withKey(f)

  /** Case-class defaults by field name. They fill in fields that older entities don't have */
  def defaults: Map[String, () => Any] = Map.empty

  /** Override the entity's logical name...
    *
    * Drives the `Class/field` prefix used for attribute keys, storage buckets, error messages, and
    * `pretty` output. The default is the case class's simple name.. use `.named("...")` to
    * disambiguate when two case classes share a simple name across packages
    */
  def withName(s: String): Entity[A]

  /** Alias for `withName`. */
  def named(s: String): Entity[A] = withName(s)
}

object Entity {
  inline def derived[A](using m: Mirror.ProductOf[A], ct: ClassTag[A]): Entity[A] = {
    val nm = constValue[m.MirroredLabel]
    val labels = constValueTuple[m.MirroredElemLabels].toList.asInstanceOf[List[String]]
    val ss = shapesOf[A, m.MirroredElemTypes]
    val base: Entity[A] = Impl[A](
      name = nm,
      clazz = ct.runtimeClass.asInstanceOf[Class[A]],
      fields = labels,
      shapes = ss,
      decomposeFn = (a: A) =>
        labels.zip(a.asInstanceOf[Product].productIterator.toList).toVector,
      reconstructFn = (attrs: Map[String, Any]) =>
        val arr: Array[Object] = labels.iterator.map(l => attrs(l).asInstanceOf[Object]).toArray
        m.fromProduct(Tuple.fromArray(arr))
      ,
      keyOf = None,
      uniques = Nil,
      defaults = Defaults.of[A]
    )
    applyFieldAnnotations[A](base)
  }

  /** Reads `@key` / `@unique` annotations on the case class fields and chains the corresponding
    * `.withKey` / `.unique` calls on the derived entity
    *
    * A no-op if no fields are annotated
    */
  inline def applyFieldAnnotations[A](base: Entity[A]): Entity[A] =
    ${ Entity.applyFieldAnnotationsImpl[A]('base) }

  private def applyFieldAnnotationsImpl[A: Type](
      base: Expr[Entity[A]]
  )(using Quotes): Expr[Entity[A]] = {
    import quotes.reflect.*
    val sym = TypeRepr.of[A].typeSymbol
    val params = sym.primaryConstructor.paramSymss.flatten.filterNot(_.isType)
    val keyTpe = TypeRepr.of[key]
    val uniqueTpe = TypeRepr.of[unique]

    def hasAnno(s: Symbol, t: TypeRepr): Boolean =
      s.annotations.exists(_.tpe =:= t)

    val keyFields = params.filter(p => hasAnno(p, keyTpe))
    val uniqueFields = params.filter(p => hasAnno(p, uniqueTpe))

    if keyFields.size > 1 then
      report.errorAndAbort(
        s"${TypeRepr.of[A].show}: more than one field marked @key (${keyFields.map(_.name).mkString(", ")}); a key is one field or computed via .key on the chain"
      )

    val withKeyApplied: Expr[Entity[A]] = keyFields.headOption match {
      case Some(f) => '{ ${ base }.withKey(${ fieldLambda[A](f.name) }) }
      case None    => base
    }

    uniqueFields.foldLeft(withKeyApplied) { (acc, f) =>
      '{ ${ acc }.unique(${ fieldLambda[A](f.name) }) }
    }
  }

  /** Builds `(a: A) => a.fieldName` via TASTy reflection, by name. */
  private def fieldLambda[A: Type](fieldName: String)(using Quotes): Expr[A => Any] = {
    import quotes.reflect.*
    val mt = MethodType(List("a"))(_ => List(TypeRepr.of[A]), _ => TypeRepr.of[Any])
    Lambda(
      owner = Symbol.spliceOwner,
      tpe = mt,
      rhsFn = (_, params) => Select.unique(params.head.asInstanceOf[Term], fieldName)
    ).asExprOf[A => Any]
  }

  private inline def shapesOf[A, T <: Tuple]: List[FieldShape] =
    inline erasedValue[T] match {
      case _: EmptyTuple => Nil
      case _: (h *: t)   => shapeOf[A, h] :: shapesOf[A, t]
    }

  private inline def shapeOf[A, T]: FieldShape =
    inline erasedValue[T] match {
      case _: Set[t]    => setShape[A, t]
      case _: Option[t] => optionShape[A, t]
      case _            => valueShape[A, T]
    }

  /** Codec for a value field, erased so all the shapes fit in one list */
  private inline def codecOf[T]: Codec[Any] = summonInline[Codec[T]].asInstanceOf[Codec[Any]]

  /* Ref detection for the three shape builders below
   *
   * The self-ref guard goes first so we don't eagerly summon `Entity[A]` inside its own derivation
   * (that path SOFs the lazy init). Self-refs and targets whose given is still initializing
   * (`ev == null`, mutually recursive entities) get a thunk using `summonInline`... so the lookup
   * waits until first use when the given is ready. The keyOf check waits with it
   */

  /** `Option[T]`... an optional ref when `T` is an entity, else an optional value */
  private inline def optionShape[A, T]: FieldShape =
    summonFrom {
      case _: (T <:< A) =>
        FieldShape.OptionalRef(() => summonInline[Entity[A]].asInstanceOf[Entity[?]])
      case ev: Entity[T] =>
        if ev == null then FieldShape.OptionalRef(() => summonInline[Entity[T]])
        else {
          require(
            ev.keyOf.isDefined,
            s"ref target ${ev.name} needs .key (used as Option[${ev.name}] field)"
          )
          FieldShape.OptionalRef(() => ev)
        }
      case _ =>
        FieldShape.Optional(codecOf[T])
    }

  private inline def setShape[A, T]: FieldShape =
    summonFrom {
      case _: (T <:< A) =>
        FieldShape.MultiRef(() => summonInline[Entity[A]].asInstanceOf[Entity[?]])
      case ev: Entity[T] =>
        if ev == null then FieldShape.MultiRef(() => summonInline[Entity[T]])
        else {
          require(
            ev.keyOf.isDefined,
            s"ref target ${ev.name} needs .key (used as Set[${ev.name}] field)"
          )
          FieldShape.MultiRef(() => ev)
        }
      case _ =>
        FieldShape.MultiValue(codecOf[T])
    }

  private inline def valueShape[A, T]: FieldShape =
    summonFrom {
      case _: (T <:< A) =>
        FieldShape.Ref(() => summonInline[Entity[A]].asInstanceOf[Entity[?]])
      case ev: Entity[T] =>
        if ev == null then FieldShape.Ref(() => summonInline[Entity[T]])
        else {
          require(ev.keyOf.isDefined, s"ref target ${ev.name} needs .key")
          FieldShape.Ref(() => ev)
        }
      case _ =>
        FieldShape.Value(codecOf[T])
    }

  final private case class Impl[A](
      name: String,
      clazz: Class[A],
      fields: List[String],
      shapes: List[FieldShape],
      decomposeFn: A => Vector[(String, Any)],
      reconstructFn: Map[String, Any] => A,
      keyOf: Option[A => Any],
      uniques: List[A => Any],
      override val defaults: Map[String, () => Any]
  ) extends Entity[A] {
    def decompose(a: A): Vector[(String, Any)] = decomposeFn(a)
    def reconstruct(attrs: Map[String, Any]): A = reconstructFn(attrs)
    def withKey(f: A => Any): Entity[A] = copy(keyOf = Some(f))
    def unique(f: A => Any): Entity[A] = copy(uniques = f :: uniques)
    def withName(s: String): Entity[A] = copy(name = s)
  }
}
