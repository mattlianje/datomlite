package datomlite

class TxBlockWhereTest extends munit.FunSuite:

  case class Person(email: String, name: String, age: Long)
  object Person:
    given Entity[Person] = Entity.derived[Person].key(_.email)

  test("b.retractWhere: predicate retract inside a tx block"):
    val db = Db(
      Person("m@x.io", "Matt", 29L),
      Person("a@x.io", "Alice", 31L)
    )
    db.tx { b =>
      b.retractWhere[Person](_.email == "m@x.io")
      b.add(Person("b@x.io", "Bob", 40L))
    }
    assertEquals(db.where[Person].run.map(_.email).toSet, Set("a@x.io", "b@x.io"))

  test("b.retractWhere: scan-based predicate works too"):
    val db = Db(
      Person("m@x.io", "Matt", 29L),
      Person("ma@x.io", "Mary", 35L),
      Person("a@x.io", "Alice", 31L)
    )
    db.tx { b =>
      b.retractWhere[Person](_.name.startsWith("M"))
    }
    assertEquals(db.where[Person].run.map(_.email), Vector("a@x.io"))

  test("b.upsertWhere: in-place update via transformation"):
    val db = Db(
      Person("m@x.io", "Matt", 29L),
      Person("a@x.io", "Alice", 31L)
    )
    db.tx { b =>
      b.upsertWhere[Person](_.email == "m@x.io")(_.copy(name = "Matthieu"))
    }
    assertEquals(
      db.where[Person](_.email == "m@x.io").one,
      Some(Person("m@x.io", "Matthieu", 29L))
    )

  test("b.upsertWhere: bumping a non-key field across many rows"):
    val db = Db(
      Person("m@x.io", "Matt", 29L),
      Person("a@x.io", "Alice", 31L)
    )
    db.tx { b =>
      b.upsertWhere[Person](_.age > 0L)(p => p.copy(age = p.age + 1L))
    }
    assertEquals(db.where[Person].run.map(_.age).toSet, Set(30L, 32L))

  test("tx block: retractWhere + upsertWhere + add all under one tx id"):
    val db = Db(
      Person("m@x.io", "Matt", 29L),
      Person("a@x.io", "Alice", 31L)
    )
    val rep = db.tx { b =>
      b.retractWhere[Person](_.email == "a@x.io")
      b.upsertWhere[Person](_.email == "m@x.io")(_.copy(name = "Matthieu"))
      b.add(Person("b@x.io", "Bob", 40L))
    }
    assertEquals(
      db.where[Person].run.map(_.email).toSet,
      Set("m@x.io", "b@x.io")
    )
    assertEquals(db.log.filter(_.t == rep.tx).map(_.t).distinct, Vector(rep.tx))

  test("db.upsertWhere: top-level update returns a TxReport"):
    val db = Db(
      Person("m@x.io", "Matt", 29L),
      Person("a@x.io", "Alice", 31L)
    )
    val rep = db.upsertWhere[Person](_.age < 30L)(_.copy(age = 100L))
    assertEquals(rep.adds.size, 1)
    assertEquals(db.where[Person](_.email == "m@x.io").one.get.age, 100L)
    assertEquals(db.where[Person](_.email == "a@x.io").one.get.age, 31L)

  test("db.upsertWhere: zero matches still produces a tx with no adds"):
    val db = Db(Person("m@x.io", "Matt", 29L))
    val rep = db.upsertWhere[Person](_.email == "nope@x.io")(_.copy(name = "X"))
    assertEquals(rep.adds, Vector.empty)
