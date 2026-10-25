package datomlite

class HistoryTest extends munit.FunSuite:

  case class Person(email: String, name: String)
  object Person:
    given Entity[Person] = Entity.derived[Person].key(_.email)

  test("log: empty db has empty log"):
    val db = Db()
    assertEquals(db.log, Vector.empty)

  test("log: each transact appends one assert datom per scalar attribute"):
    val ds = Db(Person("m@x.io", "Matt")).log
    assertEquals(ds.size, 2)
    assert(ds.forall(_.op == Op.Assert))
    assertEquals(ds.map(_.a).toSet, Set("Person/email", "Person/name"))

  test("log: retract appends Op.Retract datoms"):
    val matt = Person("m@x.io", "Matt")
    val ops = Db(matt).retract(matt).log.map(_.op)
    assertEquals(ops.count(_ == Op.Assert), 2)
    assertEquals(ops.count(_ == Op.Retract), 2)

  test("log: tx ids increase monotonically across transactions"):
    val db = Db(Person("m@x.io", "Matt")).add(Person("a@x.io", "Alice"))
    val txs = db.log.map(_.t.value).distinct
    assertEquals(txs, txs.sorted)
    assertEquals(txs.size, 2)

  test("history: returns datoms only for the matching eid"):
    val matt = Person("m@x.io", "Matt")
    val ali = Person("a@x.io", "Alice")
    val db = Db(matt, ali)
    val hist = db.historyOf(matt)
    assertEquals(hist.size, 2)
    assertEquals(hist.map(_.a).toSet, Set("Person/email", "Person/name"))
    assert(hist.forall(d => db.log.find(_.e == d.e).isDefined))

  test("history: empty when entity is not present"):
    val db = Db(Person("m@x.io", "Matt"))
    val hist = db.historyOf(Person("missing@x.io", "Nobody"))
    assertEquals(hist, Vector.empty)

  test("history: empty after the entity is fully retracted (eid no longer resolves)"):
    val matt = Person("m@x.io", "Matt")
    assertEquals(Db(matt).retract(matt).historyOf(matt), Vector.empty)

  test("history(eid): survives retraction"):
    val matt = Person("m@x.io", "Matt")
    val rep = Db().transact(Tx.add(matt))
    val eid = rep.addedEids.head
    val hist = rep.dbAfter.retract(matt).history(eid)
    assertEquals(hist.size, 4)
    assertEquals(hist.count(_.op == Op.Assert), 2)
    assertEquals(hist.count(_.op == Op.Retract), 2)

  test("history: an older db keeps its own log"):
    val db1 = Db(Person("m@x.io", "Matt"))
    val db2 = db1.add(Person("a@x.io", "Alice"))
    assertEquals(db1.log.size, 2)
    assertEquals(db2.log.size, 4)

  test("history: asOf rebuilds log up to the chosen tx"):
    val rep1 = Db().transact(Tx.add(Person("m@x.io", "Matt")))
    val db = rep1.dbAfter.add(Person("a@x.io", "Alice"))
    val past = db.asOf(rep1.tx)
    assertEquals(past.log.size, 2)
    assertEquals(past.log.map(_.t.value).toSet, Set(0L))
