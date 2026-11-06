package example

import datomlite.*

class OutsidePackageTest extends munit.FunSuite:

  case class Counter(@key name: String, n: Long) derives Entity

  test("a db's inline writes compile and run outside datomlite"):
    val db = Db(Counter("a", 0L), Counter("b", 0L))
      .upsertWhere[Counter](_.name == "a")(c => c.copy(n = c.n + 1L))
      .retractWhere[Counter](_.name == "b")
    assertEquals(db.where[Counter].run, Vector(Counter("a", 1L)))

  test("a conn's inline writes compile and run outside datomlite"):
    val conn = Conn(Db(Counter("a", 0L)))
    val rep = conn.upsertWhere[Counter](_.name == "a")(c => c.copy(n = c.n + 1L))
    assertEquals(rep.dbAfter.where[Counter].one, Some(Counter("a", 1L)))
    conn.retractWhere[Counter](_.name == "a")
    assertEquals(conn.db.count[Counter], 0L)
