package datomlite

class RangeTest extends munit.FunSuite:

  case class Person(email: String, name: String, age: Long, score: Double)
  object Person:
    given Entity[Person] = Entity.derived[Person].key(_.email)

  case class Invoice(id: String, amount: BigDecimal, lines: Int)
  object Invoice:
    given Entity[Invoice] = Entity.derived[Invoice].key(_.id)

  private def people: Db = Db(
    Person("m@x.io", "Matt", 29L, 1.5),
    Person("a@x.io", "Alice", 31L, -0.0),
    Person("b@x.io", "Bob", 40L, 0.0),
    Person("c@x.io", "Carol", 52L, 9.25),
    Person("am@x.io", "Amelie", 31L, 3.0)
  )

  private def names(q: Iterable[Person]): Set[String] = q.iterator.map(_.name).toSet

  test("where: each comparison operator, Int literal on a Long field"):
    val db = people
    assertEquals(names(db.where[Person](_.age > 31)), Set("Bob", "Carol"))
    assertEquals(names(db.where[Person](_.age >= 31)), Set("Alice", "Amelie", "Bob", "Carol"))
    assertEquals(names(db.where[Person](_.age < 31)), Set("Matt"))
    assertEquals(names(db.where[Person](_.age <= 31)), Set("Matt", "Alice", "Amelie"))

  test("where: literal on the left flips the comparison"):
    val db = people
    assertEquals(names(db.where[Person](p => 31L < p.age)), Set("Bob", "Carol"))
    assertEquals(names(db.where[Person](p => 40 >= p.age)), Set("Matt", "Alice", "Amelie", "Bob"))

  test("where: && of two bounds on one field is a between"):
    val db = people
    assertEquals(
      names(db.where[Person](p => p.age > 29L && p.age <= 40L)),
      Set("Alice", "Amelie", "Bob")
    )
    assertEquals(names(db.where[Person](p => p.age > 45L && p.age < 30L)), Set.empty[String])

  test("where: && mixes an equality probe, a range, and an opaque leaf"):
    val db = people
    assertEquals(
      names(db.where[Person](p => p.age == 31L && p.name.length > 5)),
      Set("Amelie")
    )
    assertEquals(
      names(db.where[Person](p => p.age >= 31L && p.name.contains("o"))),
      Set("Bob", "Carol")
    )

  test("where: a range slices the index instead of scanning"):
    val db = Db((1 to 1000).map(i => Person(s"$i@x.io", s"p$i", i.toLong, 0.0): Datum)*)
    var calls = 0
    val hits = db.where[Person](p => p.age > 990L && { calls += 1; true }).run
    assertEquals(hits.size, 10)
    assert(calls <= 11, s"predicate ran $calls times, expected an index slice")
    calls = 0
    assertEquals(db.where[Person](p => p.name >= "p995" && { calls += 1; true }).run.size, 5)
    assert(calls <= 6, s"string range ran the predicate $calls times")
    calls = 0
    assertEquals(
      db.where[Person](p => p.name.startsWith("p99") && { calls += 1; true }).run.size,
      11
    )
    assertEquals(calls, 11)

  test("where: String comparisons and startsWith"):
    val db = people
    assertEquals(names(db.where[Person](_.name.startsWith("A"))), Set("Alice", "Amelie"))
    assertEquals(names(db.where[Person](_.name >= "Bob")), Set("Bob", "Carol", "Matt"))
    assertEquals(
      names(db.where[Person](p => p.name.startsWith("A") && p.age > 31L)),
      Set.empty[String]
    )
    assertEquals(
      names(db.where[Person](_.name.startsWith(""))),
      Set("Matt", "Alice", "Bob", "Carol", "Amelie")
    )

  test("where: doubles keep IEEE semantics at zero"):
    val db = people
    assertEquals(
      names(db.where[Person](_.score >= 0.0)),
      Set("Matt", "Alice", "Bob", "Carol", "Amelie")
    )
    assertEquals(names(db.where[Person](_.score > 0.0)), Set("Matt", "Carol", "Amelie"))
    assertEquals(names(db.where[Person](_.score < 2)), Set("Matt", "Alice", "Bob"))

  test("where: BigDecimal and Int fields"):
    val db = Db(
      Invoice("i1", BigDecimal("10.50"), 1),
      Invoice("i2", BigDecimal("99.99"), 3),
      Invoice("i3", BigDecimal("100"), 7)
    )
    assertEquals(db.where[Invoice](_.amount >= BigDecimal(99)).run.map(_.id).toSet, Set("i2", "i3"))
    assertEquals(db.where[Invoice](_.amount < 11).run.map(_.id).toSet, Set("i1"))
    assertEquals(db.where[Invoice](_.lines > 2L).run.map(_.id).toSet, Set("i2", "i3"))

  test("count / exists / retractWhere / upsertWhere take range predicates"):
    val db = people
    assertEquals(db.count[Person](_.age > 30L), 4L)
    assert(db.exists[Person](_.age > 50L))
    assert(!db.exists[Person](_.age > 60L))
    db.upsertWhere[Person](_.age >= 40L)(p => p.copy(name = p.name.toUpperCase))
    assertEquals(names(db.where[Person](_.age >= 40L)), Set("BOB", "CAROL"))
    db.retractWhere[Person](_.age < 31L)
    assertEquals(db.count[Person], 4L)

  test("range probes follow writes and retracts"):
    val db = people
    db.retractWhere[Person](_.email == "b@x.io")
    db.add(Person("z@x.io", "Zed", 45L, 0.0))
    db.upsert(Person("m@x.io", "Matt", 60L, 1.5))
    assertEquals(names(db.where[Person](_.age > 40L)), Set("Carol", "Zed", "Matt"))

  test("typed query: range on a field, alone and as a between"):
    val db = people
    assertEquals(
      db.query[Person](p => find(p.name) where (p.age > 31L)).run.toSet,
      Set("Bob", "Carol")
    )
    assertEquals(
      db.query[Person](p => find(p.name) where (p.age >= 31L && p.age < 52L && p.score > 0.0)).run.toSet,
      Set("Amelie")
    )

  test("typed query: range under not(...) and ||"):
    val db = people
    assertEquals(
      db.query[Person](p => find(p.name) where (p.age > 30L && not(p.age > 40L))).run.toSet,
      Set("Alice", "Amelie", "Bob")
    )
    assertEquals(
      db.query[Person](p => find(p.name) where (p.age < 30L || p.age > 50L)).run.toSet,
      Set("Matt", "Carol")
    )

  final class MemStorage extends Storage:
    var datoms = Vector.empty[Datom]
    def load(): Vector[Datom] = datoms
    def append(ds: Vector[Datom]): Unit = datoms ++= ds

  object v1:
    case class Member(email: String)
    given Entity[Member] = Entity.derived[Member].key(_.email).named("Member")

  object v2:
    case class Member(email: String, level: Int = 5)
    given Entity[Member] = Entity.derived[Member].key(_.email).named("Member")

  test("range probes count a field's default for rows written before it existed"):
    val storage = MemStorage()
    locally:
      import v1.given
      Db(storage, v1.Member("old@x.io"))
    import v2.given
    val db = Db(storage, v2.Member("hi@x.io", 9), v2.Member("lo@x.io", 1))
    def emails(q: Iterable[v2.Member]) = q.iterator.map(_.email).toSet
    assertEquals(emails(db.where[v2.Member](_.level > 3)), Set("old@x.io", "hi@x.io"))
    assertEquals(emails(db.where[v2.Member](_.level > 5)), Set("hi@x.io"))
    assertEquals(emails(db.where[v2.Member](_.level <= 5)), Set("old@x.io", "lo@x.io"))
    assertEquals(
      db.query[v2.Member](m => find(m.email) where (m.level >= 5)).run.toSet,
      Set("old@x.io", "hi@x.io")
    )
    assertEquals(
      db.query[v2.Member](m => find(m.email) where (m.level > 5)).run.toSet,
      Set("hi@x.io")
    )
