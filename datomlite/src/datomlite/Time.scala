package datomlite

/** Cross-platform wall-clock timestamp (epoch millis).
  *
  * Avoids `java.time.Instant` so JS/Native don't need the scala-java-time shim
  */
opaque type Time = Long

object Time {
  def now(): Time = System.currentTimeMillis()
  def fromEpochMillis(m: Long): Time = m

  private val Iso =
    """(\d{4})-(\d{2})-(\d{2})(?:[T ](\d{2}):(\d{2}):(\d{2})(?:\.(\d+))?(Z|[+-]\d{2}:\d{2})?)?""".r

  /** Parse an ISO-8601 string.
    *
    * Accepts `YYYY-MM-DD` (midnight UTC), `YYYY-MM-DDTHH:MM:SS[.fff]`, with an optional `Z` or
    * `±HH:MM` offset
    */
  def parse(s: String): Time = s match {
    case Iso(y, mo, d, h, mi, se, frac, tz) =>
      val month = mo.toInt
      val day = d.toInt
      if (month < 1 || month > 12) fail(s, "month out of range")
      if (day < 1 || day > 31) fail(s, "day out of range")

      val hour = Option(h).fold(0)(_.toInt)
      val minute = Option(mi).fold(0)(_.toInt)
      val second = Option(se).fold(0)(_.toInt)
      if (hour > 23 || minute > 59 || second > 59) fail(s, "time out of range")

      val milli = Option(frac).fold(0) { f =>
        (if (f.length >= 3) f.substring(0, 3) else f.padTo(3, '0')).toInt
      }

      val offsetMs: Long = Option(tz) match {
        case None | Some("Z") => 0L
        case Some(o) =>
          val oh = o.substring(1, 3).toInt
          val om = o.substring(4, 6).toInt
          if (oh > 23 || om > 59) fail(s, "offset out of range")
          (oh * 60L + om) * 60_000L * (if (o.charAt(0) == '+') 1 else -1)
      }

      val days = civilToDays(y.toInt, month, day)
      val tod = (hour * 3600L + minute * 60L + second) * 1000L + milli
      fromEpochMillis(days * 86_400_000L + tod - offsetMs)

    case _ => fail(s, "malformed")
  }

  private def fail(s: String, msg: String): Nothing =
    throw new IllegalArgumentException(s"invalid ISO-8601 timestamp '$s': $msg")

  /** Days from 1970-01-01 for a proleptic-Gregorian (year, month, day)
    */
  private def civilToDays(year: Int, month: Int, day: Int): Long = {
    val y = if (month <= 2) year - 1 else year
    val era = (if (y >= 0) y else y - 399) / 400
    val yoe = y - era * 400
    val mp = if (month > 2) month - 3 else month + 9
    val doy = (153L * mp + 2) / 5 + day - 1
    val doe = yoe * 365L + yoe / 4 - yoe / 100 + doy
    era * 146097L + doe - 719468L
  }

  given Ordering[Time] = Ordering.Long

  extension (t: Time) {
    def epochMillis: Long = t
    infix def <(o: Time): Boolean = t < o
    infix def <=(o: Time): Boolean = t <= o
    infix def >(o: Time): Boolean = t > o
    infix def >=(o: Time): Boolean = t >= o
  }
}
