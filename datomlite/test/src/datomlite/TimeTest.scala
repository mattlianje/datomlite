package datomlite

class TimeTest extends munit.FunSuite:

  test("Time.parse: epoch"):
    assertEquals(Time.parse("1970-01-01").epochMillis, 0L)
    assertEquals(Time.parse("1970-01-01T00:00:00Z").epochMillis, 0L)

  test("Time.parse: date only is midnight UTC"):
    // 2026-01-15 = 20468 days since epoch
    assertEquals(Time.parse("2026-01-15").epochMillis, 20468L * 86_400_000L)

  test("Time.parse: full timestamp with Z"):
    val expected = 20468L * 86_400_000L + (10L * 3600 + 30 * 60 + 45) * 1000
    assertEquals(Time.parse("2026-01-15T10:30:45Z").epochMillis, expected)

  test("Time.parse: fractional seconds (millis)"):
    val base = 20468L * 86_400_000L
    assertEquals(Time.parse("2026-01-15T00:00:00.123Z").epochMillis, base + 123)
    // pad short fractional
    assertEquals(Time.parse("2026-01-15T00:00:00.5Z").epochMillis, base + 500)
    // truncate long fractional
    assertEquals(Time.parse("2026-01-15T00:00:00.123456Z").epochMillis, base + 123)

  test("Time.parse: positive offset shifts to UTC"):
    // 11:30+01:00 == 10:30Z
    val a = Time.parse("2026-01-15T11:30:00+01:00")
    val b = Time.parse("2026-01-15T10:30:00Z")
    assertEquals(a.epochMillis, b.epochMillis)

  test("Time.parse: negative offset shifts to UTC"):
    // 05:30-05:00 == 10:30Z
    val a = Time.parse("2026-01-15T05:30:00-05:00")
    val b = Time.parse("2026-01-15T10:30:00Z")
    assertEquals(a.epochMillis, b.epochMillis)

  test("Time.parse: no offset defaults to UTC"):
    val a = Time.parse("2026-01-15T10:30:00")
    val b = Time.parse("2026-01-15T10:30:00Z")
    assertEquals(a.epochMillis, b.epochMillis)

  test("Time.parse: round-trip ordering"):
    val t1 = Time.parse("2026-01-15T10:00:00Z")
    val t2 = Time.parse("2026-01-15T10:00:01Z")
    assert(t1 < t2)

  test("Time.parse: rejects garbage"):
    intercept[IllegalArgumentException](Time.parse("not a date"))
    intercept[IllegalArgumentException](Time.parse("2026-13-01"))
    intercept[IllegalArgumentException](Time.parse("2026-01-32"))
    intercept[IllegalArgumentException](Time.parse("2026-01-15T25:00:00Z"))
    intercept[IllegalArgumentException](Time.parse("2026-01-15T10:30:00X"))
    intercept[IllegalArgumentException](Time.parse("2026-01-15T10:30:00+1:00"))

  test("Time.parse: drives asOf"):
    case class P(@key id: String) derives Entity
    val db = Db()
    db.add(P("a"))
    // Busy-wait so the second tx lands strictly later.
    val t0 = Time.now().epochMillis
    while Time.now().epochMillis - t0 < 2 do ()
    db.add(P("b"))
    val cutoff = Time.parse("2099-01-01T00:00:00Z")
    assertEquals(db.asOf(cutoff).where[P].run.size, 2)

  test("asOf(String): String overload runs Time.parse"):
    case class P(@key id: String) derives Entity
    val db = Db()
    db.add(P("a"), P("b"))
    assertEquals(db.asOf("2099-01-01").where[P].run.size, 2)
    assertEquals(db.asOf("1970-01-01").where[P].run.size, 0)
