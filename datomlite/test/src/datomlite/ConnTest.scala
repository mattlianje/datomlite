package datomlite

class ConnTest extends munit.FunSuite:

  case class Person(email: String, name: String)
  object Person:
    given Entity[Person] = Entity.derived[Person].key(_.email)

  private val matt = Person("m@x.io", "Matt")
  private val ali = Person("a@x.io", "Alice")

  test("Conn() starts empty and in memory"):
    val conn = Conn()
    assert(conn.db eq Db.empty)

  test("Conn(db) starts from a value"):
    val conn = Conn(Db(matt))
    assertEquals(conn.db.where[Person].run, Vector(matt))

  test("writes commit and return the TxReport"):
    val conn = Conn()
    val rep = conn.add(matt, ali)
    assertEquals(rep.adds.size, 2)
    assert(conn.db eq rep.dbAfter)

  test("a db read off the conn is a value later commits don't touch"):
    val conn = Conn(Db(matt))
    val before = conn.db
    conn.add(ali)
    conn.retract(matt)
    assertEquals(before.where[Person].run, Vector(matt))
    assertEquals(conn.db.where[Person].run, Vector(ali))

  test("each report's dbBefore is the previous report's dbAfter"):
    val conn = Conn()
    val r1 = conn.add(matt)
    val r2 = conn.upsert(matt.copy(name = "Matthieu"))
    assert(r2.dbBefore eq r1.dbAfter)
    assertEquals(
      r1.dbBefore.diff(r2.dbAfter).added.map(_.v.toString).toSet,
      Set("m@x.io", "Matthieu")
    )

  test("a conn and a db share the write verbs"):
    val conn = Conn()
    conn.tx(_.add(matt))
    conn.upsertWhere[Person](_.email == "m@x.io")(_.copy(name = "Matty"))
    val db = Db().tx(_.add(matt)).upsertWhere[Person](_.email == "m@x.io")(_.copy(name = "Matty"))
    assertEquals(conn.db.where[Person].run, db.where[Person].run)

  test("a plain db can't be listened to"):
    val errs = compileErrors("Db().listen(_ => ())")
    assert(errs.contains("listen"), errs)
