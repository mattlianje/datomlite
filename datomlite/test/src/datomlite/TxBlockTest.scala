package datomlite

class TxBlockTest extends munit.FunSuite:

  case class Person(email: String, name: String)
  object Person:
    given Entity[Person] = Entity.derived[Person].key(_.email).unique(_.name)

  test("tx block: mixed add + retract + upsert in one transaction"):
    val db = Db(Person("a@x.io", "Alice"))
    val rep = db.tx { b =>
      b.add(Person("m@x.io", "Matt"))
      b.upsert(Person("a@x.io", "Alicia"))
      b.retract(Person("a@x.io", "Alicia"))
    }
    assertEquals(db.where[Person].run.toSet, Set(Person("m@x.io", "Matt")))
    assertEquals(rep.adds.size, 2)
    assertEquals(rep.retracts.size, 1)

  test("tx block: all steps share one tx id"):
    val db = Db()
    val rep = db.tx { b =>
      b.add(Person("m@x.io", "Matt"))
      b.add(Person("a@x.io", "Alice"))
      b.upsert(Person("b@x.io", "Bob"))
    }
    val txIds = db.log.map(_.t).distinct
    assertEquals(txIds, Vector(rep.tx))

  test("tx block: failure aborts the whole batch, state untouched"):
    val db = Db(Person("m@x.io", "Matt"))
    val before = db.where[Person].run.toSet
    intercept[UniqueViolation] {
      db.tx { b =>
        b.add(Person("a@x.io", "Alice"))
        b.add(Person("b@x.io", "Matt"))
      }
    }
    assertEquals(db.where[Person].run.toSet, before)

  test("tx block: empty block produces an empty tx"):
    val db = Db(Person("m@x.io", "Matt"))
    val before = db.log.size
    val rep = db.tx(_ => ())
    assertEquals(rep.adds, Vector.empty)
    assertEquals(rep.retracts, Vector.empty)
    assertEquals(db.log.size, before)

  test("tx block: retract then re-add the same key under one tx"):
    val db = Db(Person("m@x.io", "Matt"))
    val rep = db.tx { b =>
      b.retract(Person("m@x.io", "Matt"))
      b.add(Person("m@x.io", "Matthieu"))
    }
    assertEquals(db.where[Person].one, Some(Person("m@x.io", "Matthieu")))
    val touched = db.log.filter(_.t == rep.tx)
    assert(touched.exists(_.op == Op.Retract))
    assert(touched.exists(_.op == Op.Assert))

  test("Tx.batch via transact is equivalent to db.tx"):
    val db = Db()
    val rep = db.transact(Tx.batch(
      Tx.add(Person("m@x.io", "Matt")),
      Tx.add(Person("a@x.io", "Alice"))
    ))
    assertEquals(rep.adds.size, 2)
    assertEquals(db.where[Person].run.size, 2)
