package datomlite

class WatchTest extends munit.FunSuite:

  case class Department(code: String, name: String)
  object Department:
    given Entity[Department] = Entity.derived[Department].key(_.code)

  case class Employee(email: String, dept: Department, salary: Long)
  object Employee:
    given Entity[Employee] = Entity.derived[Employee].key(_.email)

  case class Note(id: String, text: String)
  object Note:
    given Entity[Note] = Entity.derived[Note].key(_.id)

  private val eng = Department("ENG", "Engineering")

  test("fires once up front with the current result"):
    val conn = Conn(Db(Employee("m@x.io", eng, 100L)))
    var seen = Vector.empty[Long]
    conn.watch(_.count[Employee])(seen :+= _)
    assertEquals(seen, Vector(1L))

  test("fires on a commit that changes the result"):
    val conn = Conn()
    var seen = Vector.empty[Long]
    conn.watch(_.count[Employee](_.salary > 50L))(seen :+= _)
    conn.add(Employee("m@x.io", eng, 100L))
    conn.add(Employee("a@x.io", eng, 10L))
    conn.add(Employee("b@x.io", eng, 70L))
    assertEquals(seen, Vector(0L, 1L, 2L))

  test("skips re-running for commits to classes the query never read"):
    val conn = Conn()
    var runs = 0
    conn.watch { s =>
      runs += 1; s.where[Employee].run.size
    }(_ => ())
    conn.add(Note("n1", "hello"))
    conn.add(Note("n2", "world"))
    assertEquals(runs, 1)
    conn.add(Employee("m@x.io", eng, 100L))
    assertEquals(runs, 2)

  test("doesn't fire when a relevant commit leaves the result unchanged"):
    val conn = Conn(Db(Employee("m@x.io", eng, 100L)))
    var seen = Vector.empty[Vector[String]]
    conn.watch(_.where[Employee](_.salary > 50L).run.map(_.email))(seen :+= _)
    conn.add(Employee("a@x.io", eng, 10L))
    assertEquals(seen, Vector(Vector("m@x.io")))

  test("reading an entity counts its ref targets as read"):
    val conn = Conn(Db(Employee("m@x.io", eng, 100L)))
    var seen = Vector.empty[Set[String]]
    conn.watch(_.where[Employee].run.map(_.dept.name).toSet)(seen :+= _)
    conn.upsert(Department("ENG", "Eng & Research"))
    assertEquals(seen, Vector(Set("Engineering"), Set("Eng & Research")))

  test("typed queries and pulls are tracked"):
    val conn = Conn(Db(Employee("m@x.io", eng, 100L)))
    var runs = 0
    var seen = Vector.empty[Set[String]]
    conn.watch { s =>
      runs += 1
      s.query[Employee](e => find(e.email) where (e.salary >= 100L)).run.toSet
    }(seen :+= _)
    conn.add(Note("n1", "unrelated"))
    conn.add(Employee("a@x.io", eng, 200L))
    assertEquals(runs, 2)
    assertEquals(seen, Vector(Set("m@x.io"), Set("m@x.io", "a@x.io")))

  test("cancel stops further callbacks"):
    val conn = Conn()
    var seen = Vector.empty[Long]
    val sub = conn.watch(_.count[Employee])(seen :+= _)
    conn.add(Employee("m@x.io", eng, 100L))
    sub.cancel()
    conn.add(Employee("a@x.io", eng, 100L))
    assertEquals(seen, Vector(0L, 1L))

  test("a throwing query doesn't break the db or other watches"):
    val conn = Conn()
    var seen = Vector.empty[Long]
    conn.watch(s =>
      if s.count[Employee] == 1L then sys.error("query boom (expected in test output)") else 0
    )(_ => ())
    conn.watch(_.count[Employee])(seen :+= _)
    conn.add(Employee("m@x.io", eng, 100L))
    conn.add(Employee("a@x.io", eng, 100L))
    assertEquals(seen, Vector(0L, 1L, 2L))

  test("watch reads the conn's db as of each commit"):
    val conn = Conn()
    var seen = Vector.empty[Long]
    conn.watch(_.count[Employee])(seen :+= _)
    val before = conn.db
    conn.add(Employee("m@x.io", eng, 100L))
    assertEquals(before.count[Employee], 0L)
    assertEquals(seen, Vector(0L, 1L))

  test("lazy results are rejected at compile time"):
    val errs = compileErrors("Conn().watch(_.where[Note])(_ => ())")
    assert(errs.contains("materialized"), errs)

  test("a plain db has nothing to watch"):
    val errs = compileErrors("Db().watch(_.count[Note])(_ => ())")
    assert(errs.contains("watch"), errs)
