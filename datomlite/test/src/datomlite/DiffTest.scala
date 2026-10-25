package datomlite

class DiffTest extends munit.FunSuite:

  case class Person(email: String, name: String)
  object Person:
    given Entity[Person] = Entity.derived[Person].key(_.email)

  test("diff: before vs after an add"):
    val before = Db(Person("m@x.io", "Matt"))
    val after = before.add(Person("a@x.io", "Alice"))

    val d = before.diff(after)
    assertEquals(d.added.map(_.v.toString).toSet, Set("a@x.io", "Alice"))
    assert(d.retracted.isEmpty)
    assert(d.added.forall(_.op == Op.Assert))

  test("diff: after vs before inverts added/retracted"):
    val before = Db(Person("m@x.io", "Matt"))
    val after = before.add(Person("a@x.io", "Alice"))

    val d = after.diff(before)
    assertEquals(d.retracted.map(_.v.toString).toSet, Set("a@x.io", "Alice"))
    assert(d.added.isEmpty)
    assert(d.retracted.forall(_.op == Op.Retract))

  test("diff: same db is empty"):
    val db = Db(Person("m@x.io", "Matt"))
    assert(db.diff(db).isEmpty)

  test("diff: a branch is a clean projection"):
    val db = Db(Person("m@x.io", "Matt"))
    val whatIf = db.add(Person("e@x.io", "Eve"))
    val d = db.diff(whatIf)
    assertEquals(d.added.map(_.v.toString).toSet, Set("e@x.io", "Eve"))
    assertEquals(db.where[Person].run.size, 1, "original untouched")

  test("diff: retract shows up as retracted on the prior side"):
    val before = Db(Person("m@x.io", "Matt"))
    val d = before.diff(before.retract(Person("m@x.io", "Matt")))
    assertEquals(d.retracted.map(_.v.toString).toSet, Set("m@x.io", "Matt"))
    assert(d.added.isEmpty)

  test("diff: upsert shows the field swap"):
    val before = Db(Person("m@x.io", "Matt"))
    val d = before.diff(before.upsert(Person("m@x.io", "Matthieu")))
    assertEquals(d.added.map(_.v.toString), Vector("Matthieu"))
    assertEquals(d.retracted.map(_.v.toString), Vector("Matt"))

  test("diff: result Datoms carry tx provenance from their source side"):
    val rep = Db(Person("m@x.io", "Matt")).transact(Tx.add(Person("a@x.io", "Alice")))
    val d = rep.dbBefore.diff(rep.dbAfter)
    assert(d.added.forall(_.t == rep.tx))

  test("diff: asOf rewinds and diff sees the gap"):
    val rep1 = Db().transact(Tx.add(Person("m@x.io", "Matt")))
    val db = rep1.dbAfter.add(Person("a@x.io", "Alice"))
    val d = db.asOf(rep1.tx).diff(db)
    assertEquals(d.added.map(_.v.toString).toSet, Set("a@x.io", "Alice"))
    assert(d.retracted.isEmpty)
