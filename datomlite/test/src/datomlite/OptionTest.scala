package datomlite

class OptionTest extends munit.FunSuite:

  case class Person(email: String, name: String, nickname: Option[String])
  object Person:
    given Entity[Person] = Entity.derived[Person].key(_.email)

  test("Option: None writes no datom for that attribute"):
    val db = Db()
    db.add(Person("m@x.io", "Matt", None))
    val datoms = db.log
    assert(datoms.forall(_.a != "Person/nickname"))
    assertEquals(datoms.map(_.a).toSet, Set("Person/email", "Person/name"))

  test("Option: Some(v) writes one datom with the inner value"):
    val db = Db()
    db.add(Person("m@x.io", "Matt", Some("matty")))
    val nick = db.log.find(_.a == "Person/nickname")
    assertEquals(nick.map(_.v), Some("matty"))

  test("Option: round-trips None"):
    val db = Db()
    val matt = Person("m@x.io", "Matt", None)
    db.add(matt)
    assertEquals(db.where[Person].run, Vector(matt))

  test("Option: round-trips Some(v)"):
    val db = Db()
    val matt = Person("m@x.io", "Matt", Some("matty"))
    db.add(matt)
    assertEquals(db.where[Person].run, Vector(matt))

  test("Option: where with .contains rides AVET"):
    val db = Db()
    db.add(
      Person("m@x.io", "Matt", Some("matty")),
      Person("a@x.io", "Alice", Some("ali")),
      Person("b@x.io", "Bob", None)
    )
    val rows = db.where[Person](_.nickname.contains("matty")).run
    assertEquals(rows.map(_.email), Vector("m@x.io"))

  test("Option: where with == Some(...) falls back to scan but works"):
    val db = Db()
    db.add(
      Person("m@x.io", "Matt", Some("matty")),
      Person("a@x.io", "Alice", None)
    )
    val rows = db.where[Person](_.nickname == Some("matty")).run
    assertEquals(rows.map(_.email), Vector("m@x.io"))

  test("Option: where with == None matches absent rows"):
    val db = Db()
    db.add(
      Person("m@x.io", "Matt", Some("matty")),
      Person("a@x.io", "Alice", None)
    )
    val rows = db.where[Person](_.nickname == None).run
    assertEquals(rows.map(_.email), Vector("a@x.io"))

  test("Option: pull returns Option[T]"):
    val db = Db()
    val rep = db.add(
      Person("m@x.io", "Matt", Some("matty")),
      Person("a@x.io", "Alice", None)
    )
    val out1 = db.pullByEid[Person](rep.addedEids.head)(_.nickname)
    assertEquals(out1, Some(Some("matty")))
    val out2 = db.pullByEid[Person](rep.addedEids(1))(_.nickname)
    assertEquals(out2, Some(None))

  test("Option: DSL Row[A].opt returns Term[T] (inner)"):
    val db = Db()
    db.add(
      Person("m@x.io", "Matt", Some("matty")),
      Person("a@x.io", "Alice", Some("ali")),
      Person("b@x.io", "Bob", None)
    )
    val rows = db.query[Person] { e =>
      find(e.name) where (e.nickname === "matty")
    }.run
    assertEquals(rows, Vector("Matt"))

  test("Option: find(e.opt) excludes rows where the attr is absent (Datalog semantics)"):
    val db = Db()
    db.add(
      Person("m@x.io", "Matt", Some("matty")),
      Person("a@x.io", "Alice", None)
    )
    val rows = db.query[Person] { e =>
      find(e.nickname)
    }.run
    assertEquals(rows.toSet, Set("matty"))

  test("Option: upsert from None to Some replaces correctly"):
    val db = Db()
    db.add(Person("m@x.io", "Matt", None))
    db.upsert(Person("m@x.io", "Matt", Some("matty")))
    assertEquals(db.where[Person].one, Some(Person("m@x.io", "Matt", Some("matty"))))

  test("Option: upsert from Some to None drops the datom"):
    val db = Db()
    db.add(Person("m@x.io", "Matt", Some("matty")))
    db.upsert(Person("m@x.io", "Matt", None))
    assertEquals(db.where[Person].one, Some(Person("m@x.io", "Matt", None)))
    assert(db.where[Person](_.nickname.contains("matty")).run.isEmpty)

  test("Option: retract works for Some-bearing rows"):
    val db = Db()
    val matt = Person("m@x.io", "Matt", Some("matty"))
    db.add(matt)
    db.retract(matt)
    assertEquals(db.where[Person].run, Vector.empty)

  test("Option: retract works for None-bearing rows"):
    val db = Db()
    val matt = Person("m@x.io", "Matt", None)
    db.add(matt)
    db.retract(matt)
    assertEquals(db.where[Person].run, Vector.empty)
