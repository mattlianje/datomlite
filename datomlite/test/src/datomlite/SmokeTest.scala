package datomlite

class SmokeTest extends munit.FunSuite:

  case class Person(name: String, email: String)
  object Person:
    given Entity[Person] = Entity.derived[Person].key(_.email).unique(_.name)

  test("Db() is queryable + empty"):
    val db = Db()
    assertEquals(db.where[Person].run, Vector.empty)

  test("Db(values*) seeds initial state, mixed types"):
    case class Department(code: String, name: String)
    given Entity[Department] = Entity.derived[Department].key(_.code)
    val matt = Person("Matt", "m@x.io")
    val eng = Department("ENG", "Engineering")
    val db = Db(matt, eng, Person("Alice", "a@x.io"))
    assertEquals(db.where[Person].run.map(_.name).toSet, Set("Matt", "Alice"))
    assertEquals(db.where[Department].run, Vector(eng))

  test("transact + find round-trip"):
    val matt = Person("Matt", "m@x.io")
    val ali = Person("Alice", "a@x.io")
    val rep = Db().transact(Tx.add(matt, ali))
    val db = rep.dbAfter
    assertEquals(rep.adds.size, 2)
    assertEquals(db.where[Person].run.toSet, Set(matt, ali))
    assertEquals(db.where[Person](_.email == "m@x.io").one, Some(matt))

  test("retract removes from current view"):
    val matt = Person("Matt", "m@x.io")
    val db = Db(matt).retract(matt)
    assertEquals(db.where[Person].run, Vector.empty)

  test("writes return a new db and leave the old one as it was"):
    val matt = Person("Matt", "m@x.io")
    val db1 = Db(matt)
    val db2 = db1.add(Person("Alice", "a@x.io"))
    assertEquals(db1.where[Person].run, Vector(matt))
    assertEquals(db2.where[Person].run.size, 2)

  test("TxReport carries the db before and after"):
    val matt = Person("Matt", "m@x.io")
    val db = Db(matt)
    val rep = db.transact(Tx.add(Person("Alice", "a@x.io")))
    assert(rep.dbBefore eq db)
    assertEquals(rep.dbAfter.where[Person].run.size, 2)

  test("Db() is one shared empty value"):
    assert(Db() eq Db.empty)
    assertEquals(Db.empty.log, Vector.empty)

  test("asOf time travel by tx"):
    val matt = Person("Matt", "m@x.io")
    val rep1 = Db().transact(Tx.add(matt))
    val db = rep1.dbAfter.add(Person("Alice", "a@x.io"))
    assertEquals(db.asOf(rep1.tx).where[Person].run, Vector(matt))

  test("asOf time travel by Time"):
    val matt = Person("Matt", "m@x.io")
    val rep1 = Db().transact(Tx.add(matt))
    val t0 = Time.now().epochMillis
    while Time.now().epochMillis - t0 < 2 do ()
    val db = rep1.dbAfter.add(Person("Alice", "a@x.io"))
    assertEquals(db.asOf(rep1.time).where[Person].run, Vector(matt))

  test("TxReport carries a wall-clock timestamp"):
    val before = Time.now()
    val rep = Db().transact(Tx.add(Person("Matt", "m@x.io")))
    val after = Time.now()
    assert(rep.time >= before && rep.time <= after)

  test("transacting inside a tx fn on a plain db is harmless"):
    val db = Db(Person("Matt", "m@x.io"))
    val db2 = db.transact(Tx.fn { s =>
      s.add(Person("Ghost", "g@x.io"))
      Tx.add(Person("Alice", "a@x.io"))
    }).dbAfter
    assertEquals(db2.where[Person].run.map(_.name).toSet, Set("Matt", "Alice"))

  test("entity carries key + uniques + class + fields"):
    val e = summon[Entity[Person]]
    assert(e.keyOf.isDefined)
    assertEquals(e.uniques.size, 1)
    assertEquals(e.clazz, classOf[Person])
    assertEquals(e.fields, List("name", "email"))

  test("entity decompose / reconstruct round-trips via Mirror"):
    val e = summon[Entity[Person]]
    val matt = Person("Matt", "m@x.io")
    val parts = e.decompose(matt)
    assertEquals(parts, Vector("name" -> "Matt", "email" -> "m@x.io"))
    assertEquals(e.reconstruct(parts.toMap), matt)

  test("find with equality predicate hits the AVET index"):
    val matt = Person("Matt", "m@x.io")
    val ali = Person("Alice", "a@x.io")
    val db = Db(matt, ali)
    assertEquals(db.where[Person](_.email == "m@x.io").run, Vector(matt))
    assertEquals(db.where[Person](_.email == "missing@x.io").run, Vector.empty)

  test("where inside a for-comprehension is a hash-probe join"):
    case class Order(email: String, item: String)
    given Entity[Order] = Entity.derived
    val matt = Person("Matt", "m@x.io")
    val ali = Person("Alice", "a@x.io")
    val db = Db(matt, ali)
      .add(Order("m@x.io", "book"), Order("a@x.io", "lamp"), Order("m@x.io", "pen"))
    val rows = (for
      p <- db.where[Person]
      o <- db.where[Order](_.email == p.email)
    yield (p.name, o.item)).run.toSet
    assertEquals(rows, Set(("Matt", "book"), ("Matt", "pen"), ("Alice", "lamp")))

  test("retract removes from AVET too"):
    val matt = Person("Matt", "m@x.io")
    val db = Db(matt)
    assertEquals(db.where[Person](_.email == "m@x.io").run, Vector(matt))
    val db2 = db.retract(matt)
    assertEquals(db2.where[Person](_.email == "m@x.io").run, Vector.empty)

  test("where: typo'd field in equality predicate is a compile error"):
    val db = Db()
    val errors: String = compileErrors("""
      db.where[Person](_.emial == "m@x.io")
    """)
    assert(
      errors.contains("emial") || errors.contains("value emial"),
      s"expected typo error, got:\n$errors"
    )

  test("where: wrong value type in equality predicate is a compile error"):
    val db = Db()
    val errors: String = compileErrors("""
      db.where[Person](_.email == 42)
    """)
    assert(errors.nonEmpty, s"expected type-mismatch error, got nothing")

  test("db.add / db.retract shortcut, mixed entity types"):
    case class Department(code: String, name: String)
    given Entity[Department] = Entity.derived[Department].key(_.code)
    val matt = Person("Matt", "m@x.io")
    val eng = Department("ENG", "Engineering")
    val db = Db().add(matt, eng)
    assertEquals(db.where[Person].run, Vector(matt))
    assertEquals(db.where[Department].run, Vector(eng))
    val db2 = db.retract(matt, eng)
    assertEquals(db2.where[Person].run, Vector.empty)
    assertEquals(db2.where[Department].run, Vector.empty)

  test("db.retractWhere by equality predicate (uses AVET)"):
    val db = Db(Person("Matt", "m@x.io"), Person("Alice", "a@x.io"), Person("Bob", "b@x.io"))
    val rep = db.transact(Tx.retractWhere[Person](_.email == "m@x.io"))
    assertEquals(rep.retracts.size, 1)
    assertEquals(rep.dbAfter.where[Person].run.map(_.name).toSet, Set("Alice", "Bob"))
    assertEquals(
      db.retractWhere[Person](_.email == "m@x.io").where[Person].run.toSet,
      rep.dbAfter.where[Person].run.toSet
    )

  test("db.retractWhere by non-equality predicate (scans + retracts all matches)"):
    case class Item(sku: Long, name: String)
    given Entity[Item] = Entity.derived[Item].key(_.sku)
    val db = Db(Item(1L, "a"), Item(2L, "b"), Item(3L, "c"))
    val rep = db.transact(Tx.retractWhere[Item](_.sku >= 2L))
    assertEquals(rep.retracts.size, 2)
    assertEquals(rep.dbAfter.where[Item].run.map(_.sku), Vector(1L))

  test("db.retractWhere with no matches → empty TxReport"):
    val db = Db(Person("Matt", "m@x.io"))
    val rep = db.transact(Tx.retractWhere[Person](_.email == "missing@x.io"))
    assertEquals(rep.retracts.size, 0)
    assertEquals(rep.dbAfter.where[Person].run.size, 1)

  test("key violation throws UniqueViolation"):
    val db = Db(Person("Matt", "m@x.io"))
    val ex = intercept[UniqueViolation](db.add(Person("Matthew", "m@x.io")))
    assertEquals(ex.entityName, "Person")
    assertEquals(ex.constraint, "key")
    assertEquals(ex.value, "m@x.io")
    assertEquals(db.where[Person].run.map(_.name), Vector("Matt"))

  test("unique violation throws UniqueViolation"):
    val db = Db(Person("Matt", "m@x.io"))
    val ex = intercept[UniqueViolation](db.add(Person("Matt", "matt2@x.io")))
    assertEquals(ex.constraint, "unique")
    assertEquals(ex.value, "Matt")
    assertEquals(db.where[Person].run.map(_.email), Vector("m@x.io"))

  test("identical row re-asserted is still a silent no-op"):
    val matt = Person("Matt", "m@x.io")
    val db = Db(matt).add(matt)
    assertEquals(db.where[Person].count, 1L)

  test("retract then re-add same key works"):
    val matt = Person("Matt", "m@x.io")
    val db = Db(matt).retract(matt).add(Person("Matty", "m@x.io"))
    assertEquals(db.where[Person].run.map(_.name), Vector("Matty"))

  test("db.where[A] no-predicate returns all"):
    val db = Db(Person("Matt", "m@x.io"), Person("Alice", "a@x.io"))
    assertEquals(db.where[Person].run.map(_.name).toSet, Set("Matt", "Alice"))

  test("db.where[A] composes in a for-comp"):
    case class Order(email: String, item: String)
    given Entity[Order] = Entity.derived
    val db = Db(Person("Matt", "m@x.io")).add(Order("m@x.io", "book"))
    val rows = (for
      p <- db.where[Person]
      o <- db.where[Order](_.email == p.email)
    yield (p.name, o.item)).run.toSet
    assertEquals(rows, Set(("Matt", "book")))
