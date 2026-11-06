package example

import datomlite.*

class ReadmeDemoTest extends munit.FunSuite:

  case class Fighter(@key name: String, style: String, country: String) derives Entity
  case class Bout(@key id: String, red: Fighter, blue: Fighter, winner: Fighter) derives Entity

  val li     = Fighter("Jet Li", "Wushu", "China")
  val gegard = Fighter("Gegard Mousasi", "Mixed", "Netherlands")
  val rory   = Fighter("Rory MacDonald", "Pankration", "Canada")

  val db = Db(
    Bout("B1", li, gegard, li),
    Bout("B2", li, rory, li),
    Bout("B3", gegard, rory, gegard),
    Bout("B4", rory, li, li)
  )

  def standings(db: Db) = db.query[Bout, Fighter] { (b, f) =>
    find(name = f.name, wins = count(b))
      .where(b.winner === f)
      .orderBy(count(b).desc)
  }.run

  test("fighters come in through the bouts' refs"):
    assertEquals(db.where[Fighter].size, 3)

  test("reads feel like a Scala collection"):
    assertEquals(db.where[Fighter](_.country == "China").map(_.name).run, Vector("Jet Li"))

  test("joins are for-comprehensions"):
    val blueCornerWins =
      (for
        f <- db.where[Fighter]
        b <- db.where[Bout](_.winner == f)
        if b.blue == f
      yield (f.name, b.id)).run
    assertEquals(blueCornerWins, Vector(("Jet Li", "B4")))

  test("typed datalog aggregates into named-tuple rows"):
    val rows = standings(db)
    assertEquals(rows.head.name, "Jet Li")
    assertEquals(rows.map(r => (r.name, r.wins)), Vector(("Jet Li", 3L), ("Gegard Mousasi", 1L)))

  test("speculative branches leave db untouched"):
    val whatIf = db.add(Bout("B5", li, gegard, gegard))
    assertEquals(standings(whatIf).map(r => (r.name, r.wins)), Vector(("Jet Li", 3L), ("Gegard Mousasi", 2L)))
    assertEquals(standings(db).map(r => (r.name, r.wins)), Vector(("Jet Li", 3L), ("Gegard Mousasi", 1L)))
    val d = db.diff(whatIf)
    assert(d.added.map(_.v).contains("B5"))
    assert(d.retracted.isEmpty)

  test("time travel to before anything was written"):
    assertEquals(db.asOf("2026-01-01").where[Fighter].size, 0)
