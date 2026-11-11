package datomlite

class ListenTest extends munit.FunSuite:

  case class Person(email: String, name: String)
  object Person:
    given Entity[Person] = Entity.derived[Person].key(_.email)

  test("cancel stops a listener"):
    val conn = Conn()
    var count = 0
    val sub = conn.listen(_ => count += 1)
    conn.add(Person("m@x.io", "Matt"))
    sub.cancel()
    conn.add(Person("a@x.io", "Alice"))
    assertEquals(count, 1)
    assert(!sub.isActive)

  test("a listener registered after a commit doesn't hear it"):
    val conn = Conn(Db(Person("m@x.io", "Matt")))
    var reps = Vector.empty[TxReport]
    conn.listen(reps :+= _)
    val rep = conn.add(Person("a@x.io", "Alice"))
    assertEquals(reps.map(_.tx), Vector(rep.tx))

  test("a throwing listener fails neither the tx nor the other listeners"):
    val conn = Conn()
    var heard = 0
    conn.listen(_ => throw new RuntimeException("listener boom (expected in test output)"))
    conn.listen(_ => heard += 1)
    conn.add(Person("m@x.io", "Matt"))
    assertEquals(heard, 1)
    assertEquals(conn.db.count[Person], 1L)

  test("a listener that writes sees its own write after the current event, in order"):
    val conn = Conn()
    var order = Vector.empty[String]
    conn.listen { rep =>
      val names = rep.adds.map(_.value.asInstanceOf[Person].name)
      order :+= s"start ${names.mkString}"
      if names == Vector("Matt") then conn.add(Person("echo@x.io", "Echo"))
      order :+= s"end ${names.mkString}"
    }
    conn.add(Person("m@x.io", "Matt"))
    assertEquals(order, Vector("start Matt", "end Matt", "start Echo", "end Echo"))
    assertEquals(conn.db.count[Person], 2L)

  test("a failed storage append aborts the tx and fires nothing"):
    var failing = false
    val storage = new Storage:
      def load(): Vector[Datom] = Vector.empty
      def append(ds: Vector[Datom]): Unit = if failing then throw new RuntimeException("disk full")
    val conn = Conn(storage)
    conn.add(Person("m@x.io", "Matt"))
    var heard = 0
    conn.listen(_ => heard += 1)
    failing = true
    intercept[RuntimeException](conn.add(Person("a@x.io", "Alice")))
    assertEquals(conn.db.where[Person].run.map(_.name), Vector("Matt"))
    assertEquals(heard, 0)
    failing = false
    conn.add(Person("a@x.io", "Alice"))
    assertEquals(heard, 1)

  test("Subscription works with Using"):
    val conn = Conn()
    var heard = 0
    scala.util.Using.resource(conn.listen(_ => heard += 1)) { _ =>
      conn.add(Person("m@x.io", "Matt"))
    }
    conn.add(Person("a@x.io", "Alice"))
    assertEquals(heard, 1)
