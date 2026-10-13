package datomlite

class ListenTest extends munit.FunSuite:

  case class Person(email: String, name: String)
  object Person:
    given Entity[Person] = Entity.derived[Person].key(_.email)

  test("cancel stops a listener"):
    val db = Db()
    var count = 0
    val sub = db.listen(_ => count += 1)
    db.add(Person("m@x.io", "Matt"))
    sub.cancel()
    db.add(Person("a@x.io", "Alice"))
    assertEquals(count, 1)
    assert(!sub.isActive)

  test("a listener registered after a commit doesn't hear it"):
    val db = Db(Person("m@x.io", "Matt"))
    var reps = Vector.empty[TxReport]
    db.listen(reps :+= _)
    val rep = db.add(Person("a@x.io", "Alice"))
    assertEquals(reps.map(_.tx), Vector(rep.tx))

  test("a throwing listener fails neither the tx nor the other listeners"):
    val db = Db()
    var heard = 0
    db.listen(_ => throw new RuntimeException("listener boom (expected in test output)"))
    db.listen(_ => heard += 1)
    db.add(Person("m@x.io", "Matt"))
    assertEquals(heard, 1)
    assertEquals(db.count[Person], 1L)

  test("a listener that writes sees its own write after the current event, in order"):
    val db = Db()
    var order = Vector.empty[String]
    db.listen { rep =>
      val names = rep.adds.map(_.value.asInstanceOf[Person].name)
      order :+= s"start ${names.mkString}"
      if names == Vector("Matt") then db.add(Person("echo@x.io", "Echo"))
      order :+= s"end ${names.mkString}"
    }
    db.add(Person("m@x.io", "Matt"))
    assertEquals(order, Vector("start Matt", "end Matt", "start Echo", "end Echo"))
    assertEquals(db.count[Person], 2L)

  test("a failed storage append aborts the tx and fires nothing"):
    var failing = false
    val storage = new Storage:
      def load(): Vector[Datom] = Vector.empty
      def append(ds: Vector[Datom]): Unit = if failing then throw new RuntimeException("disk full")
    val db = Db(storage, Person("m@x.io", "Matt"))
    var heard = 0
    db.listen(_ => heard += 1)
    failing = true
    intercept[RuntimeException](db.add(Person("a@x.io", "Alice")))
    assertEquals(db.where[Person].run.map(_.name), Vector("Matt"))
    assertEquals(heard, 0)
    failing = false
    db.add(Person("a@x.io", "Alice"))
    assertEquals(heard, 1)

  test("snapshots never fire"):
    val db = Db()
    var heard = 0
    val sub = db.snapshot.listen(_ => heard += 1)
    db.add(Person("m@x.io", "Matt"))
    assertEquals(heard, 0)
    assert(!sub.isActive)

  test("Subscription works with Using"):
    val db = Db()
    var heard = 0
    scala.util.Using.resource(db.listen(_ => heard += 1)) { _ =>
      db.add(Person("m@x.io", "Matt"))
    }
    db.add(Person("a@x.io", "Alice"))
    assertEquals(heard, 1)
