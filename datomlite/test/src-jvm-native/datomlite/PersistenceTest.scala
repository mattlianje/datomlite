package datomlite

import java.nio.file.{Files, Path}

class PersistenceTest extends munit.FunSuite:

  case class Person(email: String, name: String)
  given Entity[Person] = Entity.derived[Person].key(_.email)

  case class Department(code: String, name: String)
  given Entity[Department] = Entity.derived[Department].key(_.code)

  case class Employee(email: String, name: String, dept: Department, skills: Set[String])
  given Entity[Employee] = Entity.derived[Employee].key(_.email)

  private def tmpFile(): Path =
    val p = Files.createTempFile("datomlite-", ".dlite")
    Files.deleteIfExists(p)
    p

  test("Storage.none is the default and writes nothing"):
    val conn = Conn()
    conn.add(Person("m@x.io", "Matt"))
    assertEquals(conn.db.where[Person].count, 1L)

  test("file storage: round-trip add + restore"):
    val path = tmpFile()
    val conn1 = Conn(FileStorage(path))
    conn1.add(Person("m@x.io", "Matt"), Person("a@x.io", "Alice"))

    val db2 = Db.load(FileStorage(path))
    assertEquals(
      db2.where[Person].run.toSet,
      Set(Person("m@x.io", "Matt"), Person("a@x.io", "Alice"))
    )

  test("file storage: retract survives restart"):
    val path = tmpFile()
    val conn1 = Conn(FileStorage(path))
    conn1.add(Person("m@x.io", "Matt"), Person("a@x.io", "Alice"))
    conn1.retract(Person("m@x.io", "Matt"))

    val db2 = Db.load(FileStorage(path))
    assertEquals(db2.where[Person].run, Vector(Person("a@x.io", "Alice")))

  test("file storage: upsert replays under same eid"):
    val path = tmpFile()
    val conn1 = Conn(FileStorage(path))
    val rep = conn1.add(Person("m@x.io", "Matt"))
    val origEid = rep.addedEids.head
    conn1.upsert(Person("m@x.io", "Matthieu"))

    val db2 = Db.load(FileStorage(path))
    assertEquals(db2.where[Person].one, Some(Person("m@x.io", "Matthieu")))
    assertEquals(db2.byEid[Person](origEid), Some(Person("m@x.io", "Matthieu")))

  test("file storage: log/history preserved across restart"):
    val path = tmpFile()
    val conn1 = Conn(FileStorage(path))
    val rep1 = conn1.add(Person("m@x.io", "Matt"))
    conn1.upsert(Person("m@x.io", "Matthieu"))
    conn1.retractWhere[Person](_.email == "m@x.io")
    val origEid = rep1.addedEids.head

    val db2 = Db.load(FileStorage(path))
    assertEquals(db2.log.size, conn1.db.log.size)
    assertEquals(db2.history(origEid).size, conn1.db.history(origEid).size)

  test("file storage: refs and card-many round-trip"):
    val path = tmpFile()
    val eng = Department("ENG", "Engineering")
    val matt = Employee("m@x.io", "Matt", eng, Set("scala", "rust"))
    val ali = Employee("a@x.io", "Alice", eng, Set("scala"))

    val conn1 = Conn(FileStorage(path))
    conn1.add(matt, ali)

    val db2 = Db.load(FileStorage(path))
    assertEquals(db2.where[Employee].run.toSet, Set(matt, ali))
    assertEquals(db2.where[Department].count, 1L)
    assertEquals(db2.where[Employee](_.dept.name == "Engineering").count, 2L)

  test("file storage: tx ids continue from where they left off"):
    val path = tmpFile()
    val conn1 = Conn(FileStorage(path))
    val rep1 = conn1.add(Person("m@x.io", "Matt"))

    val conn2 = Conn(FileStorage(path))
    val rep2 = conn2.add(Person("a@x.io", "Alice"))
    assert(
      TxId.value(rep2.tx) > TxId.value(rep1.tx),
      s"expected rep2.tx (${TxId.value(rep2.tx)}) > rep1.tx (${TxId.value(rep1.tx)})"
    )

  test("file storage: asOf still works after restart"):
    val path = tmpFile()
    val conn1 = Conn(FileStorage(path))
    val rep1 = conn1.add(Person("m@x.io", "Matt"))
    conn1.add(Person("a@x.io", "Alice"))

    val db2 = Db.load(FileStorage(path))
    assertEquals(db2.asOf(rep1.tx).where[Person].run, Vector(Person("m@x.io", "Matt")))

  test("file storage: re-adding a loaded value is a no-op"):
    val path = tmpFile()
    val matt = Person("m@x.io", "Matt")
    val conn1 = Conn(FileStorage(path))
    conn1.add(matt)
    val logSize1 = conn1.db.log.size

    val conn2 = Conn(FileStorage(path))
    conn2.add(matt)
    assertEquals(conn2.db.where[Person].count, 1L)
    assertEquals(conn2.db.log.size, logSize1)
    assertEquals(Db.load(FileStorage(path)).log.size, logSize1)

  test("file storage: strings with tabs and newlines round-trip"):
    val path = tmpFile()
    val tricky = Person("m@x.io", "Matt\twith\ttabs\nand\nnewlines\\and\\backslashes")
    val conn1 = Conn(FileStorage(path))
    conn1.add(tricky)

    val db2 = Db.load(FileStorage(path))
    assertEquals(db2.where[Person].one, Some(tricky))

  test("file storage: numeric value types round-trip"):
    case class Item(sku: Long, qty: Int, price: Double, inStock: Boolean)
    given Entity[Item] = Entity.derived[Item].key(_.sku)

    val path = tmpFile()
    val items = Vector(
      Item(1L, 5, 9.99, true),
      Item(2L, 0, -1.5, false),
      Item(3L, Int.MaxValue, Double.MaxValue, true)
    )
    val conn1 = Conn(FileStorage(path))
    items.foreach(conn1.add(_))

    val db2 = Db.load(FileStorage(path))
    assertEquals(db2.where[Item].run.toSet, items.toSet)

  test("file storage: codec values (BigInt, BigDecimal, nested vectors) round-trip"):
    case class Note(title: String, body: String) derives Codec
    case class Doc(
        id: BigInt,
        price: BigDecimal,
        notes: List[Note],
        meta: Map[String, Vector[Int]],
        tag: Option[String]
    )
    given Entity[Doc] = Entity.derived[Doc].key(_.id)

    val path = tmpFile()
    val docs = Set(
      Doc(
        BigInt("123456789012345678901234567890"),
        BigDecimal("0.10"),
        List(Note("a:b", "tab\there\nline\\slash"), Note("", "3:x")),
        Map("k:1" -> Vector(1, 2), "" -> Vector.empty),
        Some("v5:hello")
      ),
      Doc(BigInt(-1), BigDecimal("1E+3"), Nil, Map.empty, None)
    )
    val conn1 = Conn(FileStorage(path))
    docs.foreach(conn1.add(_))

    val db2 = Db.load(FileStorage(path))
    assertEquals(db2.where[Doc].run.toSet, docs)
    assertEquals(db2.log, conn1.db.log)
