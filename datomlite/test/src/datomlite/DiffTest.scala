package datomlite

class DiffTest extends munit.FunSuite:

  case class Person(email: String, name: String)
  object Person:
    given Entity[Person] = Entity.derived[Person].key(_.email)

  test("diff: snapshot vs live after add"):
    val db = Db(Person("m@x.io", "Matt"))
    val snap = db.snapshot
    db.add(Person("a@x.io", "Alice"))

    val d = snap.diff(db)
    assertEquals(d.added.map(_.v.toString).toSet, Set("a@x.io", "Alice"))
    assert(d.retracted.isEmpty)
    assert(d.added.forall(_.op == Op.Assert))

  test("diff: live vs snapshot inverts added/retracted"):
    val db = Db(Person("m@x.io", "Matt"))
    val snap = db.snapshot
    db.add(Person("a@x.io", "Alice"))

    val d = db.diff(snap)
    assertEquals(d.retracted.map(_.v.toString).toSet, Set("a@x.io", "Alice"))
    assert(d.added.isEmpty)
    assert(d.retracted.forall(_.op == Op.Retract))

  test("diff: same db is empty"):
    val db = Db(Person("m@x.io", "Matt"))
    val d = db.diff(db.snapshot)
    assert(d.isEmpty)

  test("diff: withTx branch is a clean projection"):
    val db = Db(Person("m@x.io", "Matt"))
    val whatIf = db.withTx(Tx.add(Person("e@x.io", "Eve")))
    val d = db.diff(whatIf)
    assertEquals(d.added.map(_.v.toString).toSet, Set("e@x.io", "Eve"))
    assertEquals(db.where[Person].run.size, 1, "live untouched")

  test("diff: retract shows up as retracted on the prior side"):
    val db = Db(Person("m@x.io", "Matt"))
    val before = db.snapshot
    db.retract(Person("m@x.io", "Matt"))
    val d = before.diff(db)
    assertEquals(d.retracted.map(_.v.toString).toSet, Set("m@x.io", "Matt"))
    assert(d.added.isEmpty)

  test("diff: upsert shows the field swap"):
    val db = Db(Person("m@x.io", "Matt"))
    val before = db.snapshot
    db.upsert(Person("m@x.io", "Matthieu"))
    val d = before.diff(db)
    assertEquals(d.added.map(_.v.toString), Vector("Matthieu"))
    assertEquals(d.retracted.map(_.v.toString), Vector("Matt"))

  test("diff: result Datoms carry tx provenance from their source side"):
    val db = Db()
    val rep1 = db.add(Person("m@x.io", "Matt"))
    val snap = db.snapshot
    val rep2 = db.add(Person("a@x.io", "Alice"))
    val d = snap.diff(db)
    assert(d.added.forall(_.t == rep2.tx))

  test("diff: asOf rewinds and diff sees the gap"):
    val db = Db()
    val rep1 = db.add(Person("m@x.io", "Matt"))
    val rep2 = db.add(Person("a@x.io", "Alice"))
    val past = db.asOf(rep1.tx)
    val d = past.diff(db)
    assertEquals(d.added.map(_.v.toString).toSet, Set("a@x.io", "Alice"))
    assert(d.retracted.isEmpty)
