package datomlite

/** A calendar day with no time or zone, as days since 1970-01-01
  *
  * Stored as an `Int` so it sorts and range-probes like one
  */
opaque type Date = Int

object Date {

  /** Throws on a day that doesn't exist (`Date.of(2026, 2, 30)`) */
  def of(year: Int, month: Int, day: Int): Date = {
    if month < 1 || month > 12 then fail(s"$year-$month-$day", "month out of range")
    if day < 1 || day > lengthOf(year, month) then fail(s"$year-$month-$day", "day out of range")
    civilToDays(year, month, day).toInt
  }

  def fromEpochDay(d: Int): Date = d

  /** The UTC day `t` falls on */
  def at(t: Time): Date = Math.floorDiv(t.epochMillis, 86_400_000L).toInt

  /** Today in UTC */
  def today(): Date = at(Time.now())

  private val Iso = """(-?\d{4,})-(\d{2})-(\d{2})""".r

  /** `YYYY-MM-DD` */
  def parse(s: String): Date = s match {
    case Iso(y, m, d) => of(y.toInt, m.toInt, d.toInt)
    case _            => fail(s, "malformed")
  }

  private def fail(s: String, msg: String): Nothing =
    throw new IllegalArgumentException(s"invalid date '$s': $msg")

  private def isLeap(y: Int): Boolean = (y % 4 == 0 && y % 100 != 0) || y % 400 == 0

  private def lengthOf(year: Int, month: Int): Int = month match {
    case 2              => if isLeap(year) then 29 else 28
    case 4 | 6 | 9 | 11 => 30
    case _              => 31
  }

  /** Days from 1970-01-01 for a proleptic-Gregorian (year, month, day) */
  private[datomlite] def civilToDays(year: Int, month: Int, day: Int): Long = {
    val y = if month <= 2 then year - 1 else year
    val era = (if y >= 0 then y else y - 399) / 400
    val yoe = y - era * 400
    val mp = if month > 2 then month - 3 else month + 9
    val doy = (153L * mp + 2) / 5 + day - 1
    val doe = yoe * 365L + yoe / 4 - yoe / 100 + doy
    era * 146097L + doe - 719468L
  }

  /** And back */
  private def daysToCivil(days: Long): (Int, Int, Int) = {
    val z = days + 719468L
    val era = (if z >= 0 then z else z - 146096) / 146097
    val doe = z - era * 146097
    val yoe = (doe - doe / 1460 + doe / 36524 - doe / 146096) / 365
    val doy = doe - (365 * yoe + yoe / 4 - yoe / 100)
    val mp = (5 * doy + 2) / 153
    val d = (doy - (153 * mp + 2) / 5 + 1).toInt
    val m = (if mp < 10 then mp + 3 else mp - 9).toInt
    val y = (yoe + era * 400 + (if m <= 2 then 1 else 0)).toInt
    (y, m, d)
  }

  given Ordering[Date] = Ordering.Int

  extension (d: Date) {
    def epochDay: Int = d
    def year: Int = daysToCivil(d)._1
    def month: Int = daysToCivil(d)._2
    def day: Int = daysToCivil(d)._3
    def plusDays(n: Int): Date = d + n

    /** `"2026-10-13"`. A `Date` is an `Int` at runtime so `toString` won't do this */
    def iso: String = {
      val (y, m, dd) = daysToCivil(d)
      f"$y%04d-$m%02d-$dd%02d"
    }

    infix def <(o: Date): Boolean = d < o
    infix def <=(o: Date): Boolean = d <= o
    infix def >(o: Date): Boolean = d > o
    infix def >=(o: Date): Boolean = d >= o
  }
}
