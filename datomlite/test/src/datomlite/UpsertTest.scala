package datomlite

class UpsertTest extends munit.FunSuite:

  case class Person(email: String, name: String)
  object Person:
    given Entity[Person] = Entity.derived[Person].key(_.email)

  case class Department(code: String, name: String)
  object Department:
    given Entity[Department] = Entity.derived[Department].key(_.code)

  case class Worker(email: String, dept: Department)
  object Worker:
    given Entity[Worker] = Entity.derived[Worker].key(_.email)

  test("upsert: fresh insert when no key match"):
    val db = Db()
    db.upsert(Person("m@x.io", "Matt"))
    assertEquals(db.where[Person].run, Vector(Person("m@x.io", "Matt")))

  test("upsert: replaces value on key match"):
    val db = Db()
    db.add(Person("m@x.io", "Matt"))
    db.upsert(Person("m@x.io", "Matty"))
    assertEquals(db.where[Person].run, Vector(Person("m@x.io", "Matty")))

  test("upsert: identical row is a no-op (no extra log entries)"):
    val db = Db()
    db.add(Person("m@x.io", "Matt"))
    val before = db.log.size
    db.upsert(Person("m@x.io", "Matt"))
    assertEquals(db.log.size, before)

  test("upsert: preserves the eid"):
    val db = Db()
    val rep = db.add(Person("m@x.io", "Matt"))
    val originalEid = rep.addedEids.head
    val rep2 = db.upsert(Person("m@x.io", "Matty"))
    assertEquals(rep2.addedEids.head, originalEid)
    assertEquals(db.byEid[Person](originalEid), Some(Person("m@x.io", "Matty")))

  test("upsert: log records both Op.Retract and Op.Assert for the replacement"):
    val db = Db()
    db.add(Person("m@x.io", "Matt"))
    val before = db.log.size
    db.upsert(Person("m@x.io", "Matty"))
    val newRows = db.log.drop(before)
    assertEquals(newRows.count(_.op == Op.Retract), 2)
    assertEquals(newRows.count(_.op == Op.Assert), 2)

  test("upsert: AVET is updated; old value no longer queryable"):
    val db = Db()
    db.add(Person("m@x.io", "Matt"))
    db.upsert(Person("m@x.io", "Matty"))
    assertEquals(db.where[Person](_.name == "Matt").run, Vector.empty)
    assertEquals(db.where[Person](_.name == "Matty").run, Vector(Person("m@x.io", "Matty")))

  test("upsert: unique violation across other entities still throws"):
    case class Account(email: String, handle: String)
    given Entity[Account] = Entity.derived[Account].key(_.email).unique(_.handle)
    val db = Db()
    db.add(Account("m@x.io", "matt"), Account("a@x.io", "alice"))
    val ex = intercept[UniqueViolation] {
      db.upsert(Account("m@x.io", "alice"))
    }
    assertEquals(ex.constraint, "unique")

  test("upsert: rotating a unique value to the same row is fine"):
    case class Account(email: String, handle: String)
    given Entity[Account] = Entity.derived[Account].key(_.email).unique(_.handle)
    val db = Db()
    db.add(Account("m@x.io", "matt"))
    db.upsert(Account("m@x.io", "matt"))
    assertEquals(db.where[Account].run, Vector(Account("m@x.io", "matt")))

  test("upsert: changing a unique field on the same row is fine"):
    case class Account(email: String, handle: String)
    given Entity[Account] = Entity.derived[Account].key(_.email).unique(_.handle)
    val db = Db()
    db.add(Account("m@x.io", "matt"))
    db.upsert(Account("m@x.io", "matty"))
    assertEquals(db.where[Account].run, Vector(Account("m@x.io", "matty")))

  test("upsert: nested ref behaves like add (shallow)"):
    val db = Db()
    db.add(Worker("m@x.io", Department("ENG", "Engineering")))
    val ex = intercept[UniqueViolation] {
      db.upsert(Worker("m@x.io", Department("ENG", "Engineering Research")))
    }
    assertEquals(ex.entityName, "Department")

  test("upsert: without .key behaves like add"):
    case class Note(text: String)
    given Entity[Note] = Entity.derived[Note]
    val db = Db()
    db.upsert(Note("a"))
    db.upsert(Note("a"))
    db.upsert(Note("b"))
    assertEquals(db.where[Note].run.toSet, Set(Note("a"), Note("b")))

  test("Tx.upsert via transact returns TxReport with the eid"):
    val db = Db()
    val rep = db.transact(Tx.upsert(Person("m@x.io", "Matt")))
    assertEquals(rep.addedEids.size, 1)
    assertEquals(rep.adds.size, 1)

  test("upsert: history captures the full evolution of the row under one eid"):
    val db = Db()
    val rep = db.add(Person("m@x.io", "Matt"))
    val eid = rep.addedEids.head
    db.upsert(Person("m@x.io", "Matty"))
    db.upsert(Person("m@x.io", "Matthew"))
    val hist = db.history(eid)
    val nameAsserts = hist.filter(d => d.a == "Person/name" && d.op == Op.Assert).map(_.v)
    assertEquals(nameAsserts, Vector("Matt", "Matty", "Matthew"))
