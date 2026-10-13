package datomlite

class DateTest extends munit.FunSuite:

  case class Task(id: String, due: Date, at: Time)
  object Task:
    given Entity[Task] = Entity.derived[Task].key(_.id)

  private def day(i: Int): Date = Date.of(2026, 1, 1).plusDays(i)
  private def tasks(n: Int): Db =
    Db((0 until n).map(i => Task(s"t$i", day(i), Time.fromEpochMillis(i * 1000L)): Datum)*)

  test("Date: of, parse, iso and the fields agree"):
    val d = Date.of(2026, 10, 13)
    assertEquals(Date.parse("2026-10-13"), d)
    assertEquals(d.iso, "2026-10-13")
    assertEquals((d.year, d.month, d.day), (2026, 10, 13))
    assertEquals(Date.of(1970, 1, 1).epochDay, 0)
    assertEquals(Date.of(1969, 12, 31).epochDay, -1)
    assertEquals(Date.fromEpochDay(-1).iso, "1969-12-31")
    assertEquals(Date.of(2026, 2, 28).plusDays(1).iso, "2026-03-01")

  test("Date: leap years"):
    assertEquals(Date.of(2024, 2, 29).plusDays(1).iso, "2024-03-01")
    assertEquals(Date.of(2000, 2, 29).iso, "2000-02-29")
    intercept[IllegalArgumentException](Date.of(1900, 2, 29))
    intercept[IllegalArgumentException](Date.of(2026, 2, 29))

  test("Date: days that don't exist throw"):
    intercept[IllegalArgumentException](Date.of(2026, 4, 31))
    intercept[IllegalArgumentException](Date.of(2026, 13, 1))
    intercept[IllegalArgumentException](Date.of(2026, 1, 0))
    intercept[IllegalArgumentException](Date.parse("2026-1-1"))
    intercept[IllegalArgumentException](Date.parse("nope"))

  test("Date.at takes the UTC day, before 1970 too"):
    assertEquals(Date.at(Time.parse("2026-10-13T23:59:59.999Z")).iso, "2026-10-13")
    assertEquals(Date.at(Time.parse("2026-10-13T23:30:00-01:00")).iso, "2026-10-14")
    assertEquals(Date.at(Time.fromEpochMillis(-1L)).iso, "1969-12-31")

  test("Time.iso round-trips through parse"):
    val s = "2026-10-13T09:30:05.042Z"
    assertEquals(Time.parse(s).iso, s)
    assertEquals(Time.parse("2026-10-13T10:30:05.042+01:00").iso, s)
    assertEquals(Time.fromEpochMillis(-1L).iso, "1969-12-31T23:59:59.999Z")

  test("Date and Time fields round-trip through the db"):
    val t = Task("a", Date.of(2026, 10, 13), Time.parse("2026-10-13T09:30:00Z"))
    val db = Db(t)
    assertEquals(db.where[Task].one, Some(t))
    assertEquals(db.where[Task](_.due == Date.of(2026, 10, 13)).run.map(_.id), Vector("a"))

  test("where: Date and Time ranges"):
    val db = tasks(10)
    assertEquals(db.where[Task](_.due >= day(7)).run.map(_.id).toSet, Set("t7", "t8", "t9"))
    assertEquals(db.where[Task](t => t.due > day(2) && t.due <= day(4)).run.size, 2)
    assertEquals(db.where[Task](_.at < Time.fromEpochMillis(2000L)).run.size, 2)

  test("where: Date and Time ranges slice the index instead of scanning"):
    val db = tasks(1000)
    var calls = 0
    assertEquals(db.where[Task](t => t.due > day(989) && { calls += 1; true }).run.size, 10)
    assert(calls <= 11, s"date range ran the predicate $calls times")
    calls = 0
    val cut = Time.fromEpochMillis(995_000L)
    assertEquals(db.where[Task](t => t.at >= cut && { calls += 1; true }).run.size, 5)
    assert(calls <= 6, s"time range ran the predicate $calls times")

  test("typed query: Date range, orderBy and min / max"):
    val db = tasks(5)
    assertEquals(db.query[Task](t => find(t.id) where (t.due > day(2))).run.toSet, Set("t3", "t4"))
    assertEquals(
      db.query[Task](t => find(t.due, t.id).orderBy(t.due.desc).limit(2)).run.map(_._2),
      Vector("t4", "t3")
    )
    assertEquals(db.query[Task](t => find(min(t.due), max(t.due))).run, Vector((day(0), day(4))))
