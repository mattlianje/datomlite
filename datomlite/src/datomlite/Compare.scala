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
    case (x: Comparable[_], y)    => x.asInstanceOf[Comparable[Any]].compareTo(y)
    case _ =>
      throw new RuntimeException(
        s"can't compare ${a.getClass.getSimpleName} and ${b.getClass.getSimpleName}"
      )
  }
}
