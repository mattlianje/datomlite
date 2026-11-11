package datomlite

/** Numeric and string comparator shared by predicate evaluation and row sorting.
  *
  * Numbers widen across `Int` / `Long` / `Double` so a `Long` column compared with an `Int` literal
  * lines up without an explicit cast at the call site.
  */
private[datomlite] object Compare {

  def any(a: Any, b: Any): Int = (a, b) match {
    case (null, null)             => 0
    case (null, _)                => -1
    case (_, null)                => 1
    case (x: Long, y: Long)       => java.lang.Long.compare(x, y)
    case (x: Int, y: Int)         => java.lang.Integer.compare(x, y)
    case (x: Double, y: Double)   => java.lang.Double.compare(x, y)
    case (x: String, y: String)   => x.compareTo(y)
    case (x: Long, y: Int)        => java.lang.Long.compare(x, y.toLong)
    case (x: Int, y: Long)        => java.lang.Long.compare(x.toLong, y)
    case (x: Long, y: Double)     => java.lang.Double.compare(x.toDouble, y)
    case (x: Double, y: Long)     => java.lang.Double.compare(x, y.toDouble)
    case (x: Int, y: Double)      => java.lang.Double.compare(x.toDouble, y)
    case (x: Double, y: Int)      => java.lang.Double.compare(x, y.toDouble)
    case (x: Boolean, y: Boolean) => java.lang.Boolean.compare(x, y)
    case (x: Vector[?], y: Vector[?]) =>
      x.iterator.zip(y.iterator).map(any).find(_ != 0)
        .getOrElse(java.lang.Integer.compare(x.size, y.size))
    case (x, y) if isBig(x) || isBig(y) => big(x).compare(big(y))
    case (x: Comparable[_], y)          => x.asInstanceOf[Comparable[Any]].compareTo(y)
    case _ =>
      throw new RuntimeException(
        s"can't compare ${a.getClass.getSimpleName} and ${b.getClass.getSimpleName}"
      )
  }

  /** Total order over stored forms. The sorted AVET runs on it
    *
    * Agrees with `any` wherever `any` works and never throws. Different kinds (numbers, strings,
    * booleans, vectors) order by kind first so an attribute with mixed forms still indexes. All
    * numbers are one kind so `5`, `5L` and `5.0` are one key, same as `==`... and so are `0.0` and
    * `-0.0` which `any` tells apart
    */
  def total(a: Any, b: Any): Int = {
    val ka = kind(a)
    val kb = kind(b)
    if ka != kb then java.lang.Integer.compare(ka, kb)
    else
      (a, b) match {
        case (x: Vector[?], y: Vector[?]) =>
          x.iterator.zip(y.iterator).map(total).find(_ != 0)
            .getOrElse(java.lang.Integer.compare(x.size, y.size))
        /* `BigDecimal` can't hold these and `-0.0 == 0.0` has to stay one key */
        case (_: Double, _) | (_, _: Double) =>
          val (x, y) = (toDouble(a), toDouble(b))
          if x == 0.0 && y == 0.0 then 0
          else if x.isNaN || x.isInfinite || y.isNaN || y.isInfinite then
            java.lang.Double.compare(x, y)
          else any(a, b)
        case (x: Comparable[?], _) if ka == Other && a.getClass == b.getClass =>
          x.asInstanceOf[Comparable[Any]].compareTo(b)
        /* No natural order but a stable one keeps the key findable */
        case _ if ka == Other =>
          val byClass = a.getClass.getName.compareTo(b.getClass.getName)
          lazy val byText = String.valueOf(a).compareTo(String.valueOf(b))
          if byClass != 0 then byClass
          else if byText != 0 then byText
          else if a == b then 0
          else java.lang.Integer.compare(a.hashCode, b.hashCode)
        case _ => any(a, b)
      }
  }

  /** `total` as an `Ordering`, for `TreeMap` keys */
  val ordering: Ordering[Any] = (a: Any, b: Any) => total(a, b)

  private val Null = 0
  private val Number = 1
  private val Str = 2
  private val Bool = 3
  private val Vec = 4
  private val Other = 5

  private def kind(x: Any): Int = x match {
    case null                                                     => Null
    case _: Int | _: Long | _: Double | _: BigInt | _: BigDecimal => Number
    case _: String                                                => Str
    case _: Boolean                                               => Bool
    case _: Vector[?]                                             => Vec
    case _                                                        => Other
  }

  private def toDouble(x: Any): Double = x match {
    case d: Double     => d
    case l: Long       => l.toDouble
    case i: Int        => i.toDouble
    case b: BigInt     => b.toDouble
    case b: BigDecimal => b.toDouble
    case _             => 0.0
  }

  private def isBig(x: Any): Boolean = x.isInstanceOf[BigDecimal] || x.isInstanceOf[BigInt]

  /** Comparing against a `BigInt` / `BigDecimal` widens both sides to `BigDecimal` */
  private def big(x: Any): BigDecimal = x match {
    case b: BigDecimal => b
    case b: BigInt     => BigDecimal(b)
    case l: Long       => BigDecimal(l)
    case i: Int        => BigDecimal(i)
    case d: Double     => BigDecimal(d)
    case other =>
      throw new RuntimeException(s"can't compare ${other.getClass.getSimpleName} with a big number")
  }
}
