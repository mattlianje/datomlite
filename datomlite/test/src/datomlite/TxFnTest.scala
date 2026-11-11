package datomlite

class TxFnTest extends munit.FunSuite:

  case class Counter(name: String, n: Long)
  object Counter:
    given Entity[Counter] = Entity.derived[Counter].key(_.name)

  private def bump(c: Counter): Counter = c.copy(n = c.n + 1L)

  test("Tx.fn reads the state-so-far of its batch"):
    val db = Db().transact(
      Tx.batch(
        Tx.add(Counter("hits", 0L)),
        Tx.fn(s => Tx.upsert(s.where[Counter](_.name == "hits").run.map(bump)*))
      )
    ).dbAfter
    assertEquals(db.where[Counter].run, Vector(Counter("hits", 1L)))

  test("Tx.fn composes with other steps under one tx id"):
    val rep = Db(Counter("a", 1L)).transact(
      Tx.batch(
        Tx.fn(s => Tx.upsert(s.where[Counter].run.map(bump)*)),
        Tx.add(Counter("b", 0L))
      )
    )
    val db = rep.dbAfter
    assertEquals(db.where[Counter].run.toSet, Set(Counter("a", 2L), Counter("b", 0L)))
    assert(db.log.filter(_.t == rep.tx).nonEmpty)
    assertEquals(db.log.map(_.t).distinct.size, 2)

  test("b.retractWhere sees rows added by earlier steps of the same block"):
    val db = Db(Counter("keep", 0L)).tx { b =>
      b.add(Counter("tmp", 0L))
      b.retractWhere[Counter](_.name == "tmp")
    }
    assertEquals(db.where[Counter].run, Vector(Counter("keep", 0L)))

  test("b.upsertWhere sees the state-so-far"):
    val db = Db(Counter("c", 0L)).tx { b =>
      b.upsertWhere[Counter](_.name == "c")(bump)
      b.upsertWhere[Counter](_.name == "c")(bump)
    }
    assertEquals(db.where[Counter].one, Some(Counter("c", 2L)))

  test("tx block reads through b.db, not a stale copy"):
    val db = Db(Counter("c", 5L)).tx { b =>
      val cur = b.db.where[Counter](_.name == "c").one.get
      b.upsert(cur.copy(n = cur.n * 2))
    }
    assertEquals(db.where[Counter].one, Some(Counter("c", 10L)))

  test("transacting on the conn from inside a tx fn throws and leaves state untouched"):
    val conn = Conn(Db(Counter("c", 0L)))
    intercept[IllegalStateException] {
      conn.transact(Tx.fn { _ =>
        conn.add(Counter("sneaky", 0L))
        Tx.batch()
      })
    }
    assertEquals(conn.db.where[Counter].run, Vector(Counter("c", 0L)))
    conn.add(Counter("after", 0L))
    assertEquals(conn.db.count[Counter], 2L)

  test("an exception inside a tx fn aborts the commit"):
    val conn = Conn(Db(Counter("c", 0L)))
    val before = conn.db
    intercept[RuntimeException](conn.transact(Tx.fn(_ => sys.error("boom"))))
    assert(conn.db eq before)

  test("TxReport.datoms is exactly what the commit appended to the log"):
    val db = Db(Counter("c", 0L))
    val rep = db.transact(Tx.upsertWhere[Counter](_.name == "c")(bump))
    assertEquals(rep.datoms, rep.dbAfter.log.drop(db.log.size))
    assertEquals(rep.datoms.map(_.op), Vector(Op.Retract, Op.Retract, Op.Assert, Op.Assert))
