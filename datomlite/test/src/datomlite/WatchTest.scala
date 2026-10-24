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
    val db = Db(Employee("m@x.io", eng, 100L))
    var seen = Vector.empty[Long]
    db.watch(_.count[Employee])(seen :+= _)
    assertEquals(seen, Vector(1L))

  test("fires on a commit that changes the result"):
    val db = Db()
    var seen = Vector.empty[Long]
    db.watch(_.count[Employee](_.salary > 50L))(seen :+= _)
    db.add(Employee("m@x.io", eng, 100L))
    db.add(Employee("a@x.io", eng, 10L))
    db.add(Employee("b@x.io", eng, 70L))
    assertEquals(seen, Vector(0L, 1L, 2L))

  test("skips re-running for commits to classes the query never read"):
    val db = Db()
    var runs = 0
    db.watch { s =>
      runs += 1; s.where[Employee].run.size
    }(_ => ())
    db.add(Note("n1", "hello"))
    db.add(Note("n2", "world"))
    assertEquals(runs, 1)
    db.add(Employee("m@x.io", eng, 100L))
    assertEquals(runs, 2)

  test("doesn't fire when a relevant commit leaves the result unchanged"):
    val db = Db(Employee("m@x.io", eng, 100L))
    var seen = Vector.empty[Vector[String]]
    db.watch(_.where[Employee](_.salary > 50L).run.map(_.email))(seen :+= _)
    db.add(Employee("a@x.io", eng, 10L))
    assertEquals(seen, Vector(Vector("m@x.io")))

  test("reading an entity counts its ref targets as read"):
    val db = Db(Employee("m@x.io", eng, 100L))
    var seen = Vector.empty[Set[String]]
    db.watch(_.where[Employee].run.map(_.dept.name).toSet)(seen :+= _)
    db.upsert(Department("ENG", "Eng & Research"))
    assertEquals(seen, Vector(Set("Engineering"), Set("Eng & Research")))

  test("typed queries and pulls are tracked"):
    val db = Db(Employee("m@x.io", eng, 100L))
    var runs = 0
    var seen = Vector.empty[Set[String]]
    db.watch { s =>
      runs += 1
      s.query[Employee](e => find(e.email) where (e.salary >= 100L)).run.toSet
    }(seen :+= _)
    db.add(Note("n1", "unrelated"))
    db.add(Employee("a@x.io", eng, 200L))
    assertEquals(runs, 2)
    assertEquals(seen, Vector(Set("m@x.io"), Set("m@x.io", "a@x.io")))

  test("cancel stops further callbacks"):
    val db = Db()
    var seen = Vector.empty[Long]
    val sub = db.watch(_.count[Employee])(seen :+= _)
    db.add(Employee("m@x.io", eng, 100L))
    sub.cancel()
    db.add(Employee("a@x.io", eng, 100L))
    assertEquals(seen, Vector(0L, 1L))

  test("a throwing query doesn't break the db or other watches"):
    val db = Db()
    var seen = Vector.empty[Long]
    db.watch(s =>
      if s.count[Employee] == 1L then sys.error("query boom (expected in test output)") else 0
    )(_ => ())
    db.watch(_.count[Employee])(seen :+= _)
    db.add(Employee("m@x.io", eng, 100L))
    db.add(Employee("a@x.io", eng, 100L))
    assertEquals(seen, Vector(0L, 1L, 2L))

  test("a watch on a snapshot runs once"):
    val db = Db(Employee("m@x.io", eng, 100L))
    var seen = Vector.empty[Long]
    val sub = db.snapshot.watch(_.count[Employee])(seen :+= _)
    db.add(Employee("a@x.io", eng, 100L))
    assertEquals(seen, Vector(1L))
    assert(!sub.isActive)

  test("lazy results are rejected at compile time"):
    val errs = compileErrors("Db().watch(_.where[Note])(_ => ())")
    assert(errs.contains("materialized"), errs)
