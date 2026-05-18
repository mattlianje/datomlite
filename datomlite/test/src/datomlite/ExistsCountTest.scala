package datomlite

class ExistsCountTest extends munit.FunSuite:

  case class Person(email: String, name: String)
  object Person:
    given Entity[Person] = Entity.derived[Person].key(_.email)

  test("exists: predicate match returns true"):
    val db = Db(Person("m@x.io", "Matt"), Person("a@x.io", "Alice"))
    assert(db.exists[Person](_.email == "m@x.io"))

  test("exists: predicate miss returns false"):
    val db = Db(Person("m@x.io", "Matt"))
    assert(!db.exists[Person](_.email == "nope@x.io"))

  test("exists: scan-based predicate also works"):
    val db = Db(Person("m@x.io", "Matt"), Person("a@x.io", "Alice"))
    assert(db.exists[Person](_.name.startsWith("M")))
    assert(!db.exists[Person](_.name.startsWith("Z")))

  test("exists: no-arg returns true when any row exists"):
    val empty = Db()
    assert(!empty.exists[Person])
    val db = Db(Person("m@x.io", "Matt"))
    assert(db.exists[Person])

  test("count: predicate match returns row count"):
    val db = Db(
      Person("m@x.io", "Matt"),
      Person("a@x.io", "Alice"),
      Person("b@x.io", "Bob")
    )
    assertEquals(db.count[Person](_.email == "m@x.io"), 1L)

  test("count: scan predicate counts matches"):
    val db = Db(
      Person("m@x.io", "Matthieu"),
      Person("a@x.io", "Alice"),
      Person("ma@x.io", "Mary")
    )
    assertEquals(db.count[Person](_.name.startsWith("M")), 2L)

  test("count: no-arg counts every row"):
    val db = Db(Person("m@x.io", "Matt"), Person("a@x.io", "Alice"))
    assertEquals(db.count[Person], 2L)

  test("count: zero on no match"):
    val db = Db(Person("m@x.io", "Matt"))
    assertEquals(db.count[Person](_.email == "nope@x.io"), 0L)
