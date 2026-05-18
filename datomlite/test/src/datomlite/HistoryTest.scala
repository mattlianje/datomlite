package datomlite

class HistoryTest extends munit.FunSuite:

  case class Person(email: String, name: String)
  object Person:
    given Entity[Person] = Entity.derived[Person].key(_.email)

  test("log: empty db has empty log"):
    val db = Db()
    assertEquals(db.log, Vector.empty)

  test("log: each transact appends one assert datom per scalar attribute"):
    val db = Db()
    db.add(Person("m@x.io", "Matt"))
    val ds = db.log
    assertEquals(ds.size, 2)
    assert(ds.forall(_.op == Op.Assert))
    assertEquals(ds.map(_.a).toSet, Set("Person/email", "Person/name"))

  test("log: retract appends Op.Retract datoms"):
    val db = Db()
    val matt = Person("m@x.io", "Matt")
    db.add(matt)
    db.retract(matt)
    val ops = db.log.map(_.op)
    assertEquals(ops.count(_ == Op.Assert), 2)
    assertEquals(ops.count(_ == Op.Retract), 2)

  test("log: tx ids increase monotonically across transactions"):
    val db = Db()
    db.add(Person("m@x.io", "Matt"))
    db.add(Person("a@x.io", "Alice"))
    val txs = db.log.map(_.t.value).distinct
    assertEquals(txs, txs.sorted)
    assertEquals(txs.size, 2)

  test("history: returns datoms only for the matching eid"):
    val db = Db()
    val matt = Person("m@x.io", "Matt")
    val ali = Person("a@x.io", "Alice")
    db.add(matt, ali)
    val hist = db.historyOf(matt)
    assertEquals(hist.size, 2)
    assertEquals(hist.map(_.a).toSet, Set("Person/email", "Person/name"))
    assert(hist.forall(d => db.log.find(_.e == d.e).isDefined))

  test("history: empty when entity is not present"):
    val db = Db()
    db.add(Person("m@x.io", "Matt"))
    val hist = db.historyOf(Person("missing@x.io", "Nobody"))
    assertEquals(hist, Vector.empty)

  test("history: empty after the entity is fully retracted (eid no longer resolves)"):
    val db = Db()
    val matt = Person("m@x.io", "Matt")
    db.add(matt)
    db.retract(matt)
    assertEquals(db.historyOf(matt), Vector.empty)

  test("history(eid): survives retraction"):
    val db = Db()
    val matt = Person("m@x.io", "Matt")
    val rep = db.add(matt)
    val eid = rep.addedEids.head
    db.retract(matt)
    val hist = db.history(eid)
    assertEquals(hist.size, 4)
    assertEquals(hist.count(_.op == Op.Assert), 2)
    assertEquals(hist.count(_.op == Op.Retract), 2)

  test("history: snapshot freezes the history view"):
    val db = Db()
    val matt = Person("m@x.io", "Matt")
    db.add(matt)
    val snap = db.snapshot
    db.add(Person("a@x.io", "Alice"))
    assertEquals(snap.log.size, 2)
    assertEquals(db.log.size, 4)

  test("history: asOf rebuilds log up to the chosen tx"):
    val db = Db()
    val rep1 = db.add(Person("m@x.io", "Matt"))
    db.add(Person("a@x.io", "Alice"))
    val past = db.asOf(rep1.tx)
    assertEquals(past.log.size, 2)
    assertEquals(past.log.map(_.t.value).toSet, Set(0L))
