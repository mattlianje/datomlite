package datomlite

import scala.annotation.implicitNotFound
import scala.compiletime.{constValue, constValueTuple, erasedValue, summonFrom, summonInline}
import scala.deriving.Mirror

/** How a field of type `T` is stored in a datom
  *
  * Datoms only hold a few stored forms... `String`, `Long`, `Int`, `Double`, `Boolean`, `BigInt`,
  * `BigDecimal` and `Vector`s of those. That keeps AVET, comparisons and storage backends simple.
  * Everything else maps onto them through a `Codec`
  *
  * {{{
  *   enum Status derives Codec { case Active, Banned } // stored as "Active" / "Banned"
  *   case class Address(street: String, city: String) derives Codec
  *   given Codec[Instant] = Codec.of[Instant, Long](_.toEpochMilli, Instant.ofEpochMilli)
  * }}}
  */
@implicitNotFound(
  "No Codec[${T}]. Case classes and enums stored as values need `derives Codec`; for other types " +
    "define `given Codec[${T}] = Codec.of[${T}, P](to, from)` where P already has a Codec"
)
trait Codec[T] {

  /** Value to stored form */
  def encode(t: T): Any

  /** Stored form back to a value */
  def decode(v: Any): T
}

object Codec {
  def apply[T](using c: Codec[T]): Codec[T] = c

  /** Store `T` as some `P` that already has a codec. `P` can itself be built with `of` */
  def of[T, P](to: T => P, from: P => T)(using p: Codec[P]): Codec[T] = new Codec[T] {
    def encode(t: T): Any = p.encode(to(t))
    def decode(v: Any): T = from(p.decode(v))
  }

  /** Refs just store the target eid */
  private[datomlite] val eid: Codec[Any] = new Codec[Any] {
    def encode(t: Any): Any = t
    def decode(v: Any): Any = v
  }

  /** A stored form stored as-is. `read` can widen older numbers too so a field that went from `Int`
    * to `Long` still reads what's in the log
    */
  private def stored[T](name: String)(read: PartialFunction[Any, T]): Codec[T] = new Codec[T] {
    def encode(t: T): Any = t
    def decode(v: Any): T = read.applyOrElse(v, mismatch(name))
  }

  private def mismatch(name: String)(v: Any): Nothing =
    throw new IllegalStateException(s"cannot decode stored value '$v' as $name")

  given Codec[String] = stored("String") { case s: String => s }
  given Codec[Boolean] = stored("Boolean") { case b: Boolean => b }
  given Codec[Int] = stored("Int") { case i: Int => i }
  given Codec[Long] = stored("Long") {
    case l: Long => l
    case i: Int  => i.toLong
  }
  given Codec[Double] = stored("Double") {
    case d: Double => d
    case l: Long   => l.toDouble
    case i: Int    => i.toDouble
  }
  given Codec[BigInt] = stored("BigInt") {
    case b: BigInt => b
    case l: Long   => BigInt(l)
    case i: Int    => BigInt(i)
  }
  given Codec[BigDecimal] = stored("BigDecimal") {
    case b: BigDecimal => b
    case b: BigInt     => BigDecimal(b)
    case l: Long       => BigDecimal(l)
    case i: Int        => BigDecimal(i)
    case d: Double     => BigDecimal(d)
  }

  given Codec[Float] = of[Float, Double](_.toDouble, _.toFloat)
  given Codec[Short] = of[Short, Int](_.toInt, _.toShort)
  given Codec[Byte] = of[Byte, Int](_.toInt, _.toByte)
  given Codec[Char] = of[Char, String](_.toString, _.charAt(0))
  given Codec[java.util.UUID] = of[java.util.UUID, String](_.toString, java.util.UUID.fromString)
  given Codec[Time] = of[Time, Long](_.epochMillis, Time.fromEpochMillis)
  given Codec[Date] = of[Date, Int](_.epochDay, Date.fromEpochDay)

  /* Collections store as Vectors. Unordered ones sort their elements so equal values always store
   * the same... AVET and matching rely on that */

  given [T](using c: Codec[T]): Codec[Vector[T]] = seqOf[T, Vector[T]](c, identity)
  given [T](using c: Codec[T]): Codec[List[T]] = seqOf[T, List[T]](c, _.toList)
  given [T](using c: Codec[T]): Codec[Seq[T]] = seqOf[T, Seq[T]](c, identity)

  /** Only for a `Set` inside a value. A `Set` field on an entity is card-many */
  given [T](using c: Codec[T]): Codec[Set[T]] = new Codec[Set[T]] {
    def encode(t: Set[T]): Any = sorted(t.iterator.map(c.encode))
    def decode(v: Any): Set[T] = elems(v, "Set").map(c.decode).toSet
  }

  given [K, V](using k: Codec[K], v: Codec[V]): Codec[Map[K, V]] = new Codec[Map[K, V]] {
    def encode(t: Map[K, V]): Any =
      sorted(t.iterator.map((a, b) => Vector(k.encode(a), v.encode(b))))
    def decode(x: Any): Map[K, V] = elems(x, "Map").map {
      case Vector(a, b) => k.decode(a) -> v.decode(b)
      case other        => mismatch("Map entry")(other)
    }.toMap
  }

  /** Only for an `Option` inside a value. An `Option` field on an entity is just an optional
    * attribute and `None` writes no datom
    */
  given [T](using c: Codec[T]): Codec[Option[T]] = new Codec[Option[T]] {
    def encode(t: Option[T]): Any = t.map(c.encode).toVector
    def decode(v: Any): Option[T] = elems(v, "Option").headOption.map(c.decode)
  }

  given [L, R](using l: Codec[L], r: Codec[R]): Codec[Either[L, R]] = new Codec[Either[L, R]] {
    def encode(t: Either[L, R]): Any =
      t.fold(a => Vector("Left", l.encode(a)), b => Vector("Right", r.encode(b)))
    def decode(v: Any): Either[L, R] = v match {
      case Vector("Left", a)  => Left(l.decode(a))
      case Vector("Right", b) => Right(r.decode(b))
      case other              => mismatch("Either")(other)
    }
  }

  inline given [T <: Tuple](using m: Mirror.ProductOf[T]): Codec[T] = derived[T]

  /** Codec for a case class, sealed trait or enum
    *
    * Case classes store as `Vector(name1, value1, name2, value2, ...)` and read back by field
    * name.. so older values survive added fields (with a default), dropped fields and reordering.
    * Plain enums store as the bare case name. Other sums store as `Vector(caseName, payload)`
    */
  inline def derived[T](using m: Mirror.Of[T]): Codec[T] =
    inline m match {
      case p: Mirror.ProductOf[T] =>
        new ProductCodec[T](
          constValue[p.MirroredLabel],
          p,
          constValueTuple[p.MirroredElemLabels].toList.asInstanceOf[List[String]],
          codecsOf[p.MirroredElemTypes],
          Defaults.of[T]
        )
      case s: Mirror.SumOf[T] =>
        new SumCodec[T](
          constValue[s.MirroredLabel],
          s,
          constValueTuple[s.MirroredElemLabels].toList.asInstanceOf[List[String]],
          caseCodecsOf[s.MirroredElemTypes]
        )
    }

  private inline def codecsOf[Ts <: Tuple]: List[Codec[Any]] =
    inline erasedValue[Ts] match {
      case _: EmptyTuple => Nil
      case _: (t *: ts)  => summonInline[Codec[t]].asInstanceOf[Codec[Any]] :: codecsOf[ts]
    }

  /** Sum cases rarely have their own codec so derive one if missing */
  private inline def caseCodecsOf[Ts <: Tuple]: List[Codec[Any]] =
    inline erasedValue[Ts] match {
      case _: EmptyTuple => Nil
      case _: (t *: ts) =>
        val c: Codec[t] = summonFrom {
          case c: Codec[`t`]     => c
          case m: Mirror.Of[`t`] => derived[t](using m)
        }
        c.asInstanceOf[Codec[Any]] :: caseCodecsOf[ts]
    }

  /** Built by `derived`. Public only because the inline expansion builds it at the call site
    *
    * Field codecs are by-name so a recursive type can refer to its own codec
    */
  final class ProductCodec[T](
      name: String,
      m: Mirror.ProductOf[T],
      labels: List[String],
      fieldCodecs: => List[Codec[Any]],
      defaults: Map[String, () => Any]
  ) extends Codec[T] {
    private lazy val fields = labels.zip(fieldCodecs).toArray
    def arity: Int = labels.size

    def encode(t: T): Any =
      fields.iterator.zip(t.asInstanceOf[Product].productIterator).flatMap {
        case ((label, c), v) => Iterator(label, c.encode(v))
      }.toVector

    def decode(v: Any): T = {
      val byName = elems(v, name).grouped(2).collect { case Vector(k: String, x) => k -> x }.toMap
      val values = fields.map { (label, c) =>
        byName.get(label) match {
          case Some(x) => c.decode(x)
          case None =>
            defaults.get(label).fold(throw new MissingField(name, label, None))(_())
        }
      }
      m.fromProduct(Tuple.fromArray(values.map(_.asInstanceOf[Object])))
    }
  }

  /** Built by `derived`. Public only because the inline expansion builds it at the call site */
  final class SumCodec[T](
      name: String,
      m: Mirror.SumOf[T],
      labels: List[String],
      caseCodecs: => List[Codec[Any]]
  ) extends Codec[T] {
    private lazy val cases = caseCodecs.toArray
    private lazy val indexOf = labels.zipWithIndex.toMap
    private lazy val bare = cases.forall {
      case p: ProductCodec[?] => p.arity == 0
      case _                  => false
    }

    def encode(t: T): Any = {
      val i = m.ordinal(t)
      if bare then labels(i) else Vector(labels(i), cases(i).encode(t))
    }

    def decode(v: Any): T = {
      val (label, payload) = v match {
        case s: String if bare             => (s, Vector.empty)
        case Vector(s: String, p) if !bare => (s, p)
        case other                         => mismatch(name)(other)
      }
      val i = indexOf.getOrElse(label, mismatch(name)(v))
      cases(i).decode(payload).asInstanceOf[T]
    }
  }

  private def seqOf[T, C <: Seq[T]](c: Codec[T], build: Vector[T] => C): Codec[C] = new Codec[C] {
    def encode(t: C): Any = t.iterator.map(c.encode).toVector
    def decode(v: Any): C = build(elems(v, "Seq").map(c.decode))
  }

  private def sorted(it: Iterator[Any]): Vector[Any] =
    it.toVector.sortWith((a, b) => Compare.any(a, b) < 0)

  private def elems(v: Any, name: String): Vector[Any] = v match {
    case vs: Vector[?] => vs
    case other         => mismatch(name)(other)
  }
}
