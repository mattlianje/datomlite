package datomlite

class DslTest extends munit.FunSuite:

  case class Department(code: String, name: String)
  object Department:
    given Entity[Department] = Entity.derived[Department].key(_.code)

  case class Employee(email: String, name: String, dept: Department, salary: Long)
  object Employee:
    given Entity[Employee] = Entity.derived[Employee].key(_.email)

  case class Order(buyer: String, item: String, price: Long)
  object Order:
    given Entity[Order] = Entity.derived

  private def seeded: Db =
    val db = Db()
    db.add(
      Employee("m@x.io", "Matt", Department("ENG", "Engineering"), 120_000L),
      Employee("a@x.io", "Alice", Department("ENG", "Engineering"), 90_000L),
      Employee("b@x.io", "Bob", Department("SAL", "Sales"), 150_000L)
    )
    db.add(
      Order("m@x.io", "book", 20L),
      Order("a@x.io", "lamp", 90L),
      Order("b@x.io", "chair", 60L)
    )
    db

  test("single row: no where binds every row"):
    val db = seeded
    val rows = db.query[Employee] { e =>
      find(e.name)
    }.run.toSet
    assertEquals(rows, Set("Matt", "Alice", "Bob"))

  test("single row: equality on a value field"):
    val db = seeded
    val rows = db.query[Employee] { e =>
      find(e.name) where (e.email === "m@x.io")
    }.run
    assertEquals(rows, Vector("Matt"))

  test("single row: predicate filter"):
    val db = seeded
    val rows = db.query[Employee] { e =>
      find(e.name) where (e.salary > 100_000L)
    }.run.toSet
    assertEquals(rows, Set("Matt", "Bob"))

  test("single row: && composes constraints"):
    val db = seeded
    val rows = db.query[Employee] { e =>
      find(e.name) where (e.salary > 100_000L && e.salary < 140_000L)
    }.run
    assertEquals(rows, Vector("Matt"))

  test("|| on a single field unions matches"):
    val db = seeded
    val rows = db.query[Employee] { e =>
      find(e.name) where (e.email === "m@x.io" || e.email === "b@x.io")
    }.run.toSet
    assertEquals(rows, Set("Matt", "Bob"))

  test("|| distributes over &&"):
    val db = seeded
    val rows = db.query[Employee] { e =>
      find(e.name) where (
        e.salary > 100_000L && (e.email === "m@x.io" || e.email === "b@x.io")
      )
    }.run.toSet
    assertEquals(rows, Set("Matt", "Bob"))

  test("|| keeps duplicates by default; .distinct dedupes"):
    val db = seeded
    val sel = db.query[Employee] { e =>
      find(e.name) where (e.salary > 80_000L || e.salary > 100_000L)
    }
    assertEquals(sel.run.size, 5)
    assertEquals(sel.distinct.run.toSet, Set("Matt", "Alice", "Bob"))

  test("two rows: ref equality (e.dept === d) joins"):
    val db = seeded
    val rows = db.query[Employee, Department] { (e, d) =>
      find(e.email, d.name) where (
        e.dept === d &&
          d.name === "Engineering"
      )
    }.run.toSet
    assertEquals(rows, Set(("m@x.io", "Engineering"), ("a@x.io", "Engineering")))

  test("two rows: cross-entity join via shared scalar"):
    val db = seeded
    val rows = db.query[Employee, Order] { (e, o) =>
      find(e.name, o.item) where (
        e.email === o.buyer &&
          o.price > 50L
      )
    }.run.toSet
    assertEquals(rows, Set(("Alice", "lamp"), ("Bob", "chair")))


  test("named tuple: arity 2 returns NamedTuple with user labels"):
    val db = seeded
    val rows = db.query[Employee] { e =>
      find(name = e.name, salary = e.salary) where (e.salary > 100_000L)
    }.run.toSet
    assertEquals(rows.map(r => (r.name, r.salary)), Set(("Matt", 120_000L), ("Bob", 150_000L)))

  test("named tuple: works across joins"):
    val db = seeded
    val rows = db.query[Employee, Department] { (e, d) =>
      find(email = e.email, dept = d.name) where (
        e.dept === d && d.name === "Engineering"
      )
    }.run.toSet
    assertEquals(rows.map(_.email), Set("m@x.io", "a@x.io"))
    assertEquals(rows.map(_.dept), Set("Engineering"))


  test("rules: factored constraints compose"):
    val db = seeded

    def engineers(d: Row[Department]): Constraint =
      d.name === "Engineering"

    def expensive(e: Row[Employee], min: Long): Constraint =
      e.salary > min

    val rows = db.query[Employee, Department] { (e, d) =>
      find(e.email) where (
        engineers(d) && expensive(e, 100_000L) && e.dept === d
      )
    }.run
    assertEquals(rows, Vector("m@x.io"))

  test("schema typo: bad field name is a compile error"):
    val errors = compileErrors("""
      val db = datomlite.Db()
      db.query[DslTest.this.Employee] { e =>
        find(e.emial) where (e.email === "x")
      }
    """)
    assert(
      errors.contains("emial") && errors.contains("not a field"),
      s"expected accessor-not-found error, got:\n$errors"
    )

  test("type mismatch: wrong-typed value is a compile error"):
    val errors = compileErrors("""
      val db = datomlite.Db()
      db.query[DslTest.this.Employee] { e =>
        find(e.email) where (e.email === 42)
      }
    """)
    assert(
      errors.nonEmpty,
      s"expected type-mismatch error, got:\n$errors"
    )

  test("agg: count over a single row"):
    val db = seeded
    val rows = db.query[Employee] { e =>
      find(count(e)) where (e.email === e.email)
    }.run
    assertEquals(rows, Vector(3L))

  test("agg: sum grouped by another find var"):
    val db = seeded
    val rows = db.query[Employee, Order] { (e, o) =>
      find(e.name, sum(o.price)) where (
        e.email === o.buyer
      )
    }.run.toSet
    assertEquals(rows, Set(("Matt", 20L), ("Alice", 90L), ("Bob", 60L)))

  test("agg: avg returns Double"):
    val db = seeded
    val rows = db.query[Employee] { e =>
      find(avg(e.salary)) where (e.email === e.email)
    }.run
    assertEqualsDouble(rows.head, (120_000.0 + 90_000.0 + 150_000.0) / 3.0, 0.0001)

  test("agg: min and max"):
    val db = seeded
    val rows = db.query[Employee] { e =>
      find(min(e.salary), max(e.salary)) where (e.email === e.email)
    }.run
    assertEquals(rows.head, (90_000L, 150_000L))

  test("agg: count vs countDistinct"):
    val db = Db()
    db.add(
      Order("m@x.io", "book", 20L),
      Order("m@x.io", "pen", 5L),
      Order("a@x.io", "book", 20L)
    )
    val rows = db.query[Order] { o =>
      find(count(o), countDistinct(o.item)) where (o.buyer === o.buyer)
    }.run
    assertEquals(rows.head, (3L, 2L))

  test("agg: combined with predicate filter"):
    val db = seeded
    val rows = db.query[Employee] { e =>
      find(count(e)) where (e.salary > 100_000L)
    }.run
    assertEquals(rows, Vector(2L))

  test("agg: sum on non-numeric field is a compile error"):
    val errors = compileErrors("""
      val db = datomlite.Db()
      db.query[DslTest.this.Employee] { e =>
        find(sum(e.email)) where (e.email === e.email)
      }
    """)
    assert(
      errors.contains("Numeric") || errors.contains("String"),
      s"expected Numeric constraint failure, got:\n$errors"
    )

  test("groupBy.agg: sum of price per buyer"):
    val db = seeded
    val totals = db.where[Order].groupBy(_.buyer).agg(_.map(_.price).sum)
    assertEquals(totals, Map("m@x.io" -> 20L, "a@x.io" -> 90L, "b@x.io" -> 60L))

  test("groupBy.agg: count per buyer via .size"):
    val db = seeded
    val counts = db.where[Order].groupBy(_.buyer).agg(_.size)
    assertEquals(counts, Map("m@x.io" -> 1, "a@x.io" -> 1, "b@x.io" -> 1))

  test("groupBy.agg: composes with where predicate first"):
    val db = seeded
    val totals = db.where[Order](_.price > 10L).groupBy(_.buyer).agg(_.map(_.price).sum)
    assertEquals(totals, Map("m@x.io" -> 20L, "a@x.io" -> 90L, "b@x.io" -> 60L))

  test("countBy: shortcut for groupBy then size per group"):
    val db = seeded
    val counts = db.where[Order].countBy(_.buyer)
    assertEquals(counts, Map("m@x.io" -> 1L, "a@x.io" -> 1L, "b@x.io" -> 1L))

  test("orderBy: ascending by salary"):
    val db = seeded
    val rows = db.query[Employee] { e =>
      find(e.name, e.salary).orderBy(e.salary.asc)
    }.run
    assertEquals(rows, Vector(("Alice", 90_000L), ("Matt", 120_000L), ("Bob", 150_000L)))

  test("orderBy: descending by salary"):
    val db = seeded
    val rows = db.query[Employee] { e =>
      find(e.name, e.salary).orderBy(e.salary.desc)
    }.run
    assertEquals(rows, Vector(("Bob", 150_000L), ("Matt", 120_000L), ("Alice", 90_000L)))

  test("orderBy: multi-key, dept then -salary"):
    val db = seeded
    val rows = db.query[Employee, Department] { (e, d) =>
      find(d.name, e.name, e.salary)
        .where(e.dept === d)
        .orderBy(d.name.asc, e.salary.desc)
    }.run
    assertEquals(
      rows,
      Vector(
        ("Engineering", "Matt", 120_000L),
        ("Engineering", "Alice", 90_000L),
        ("Sales", "Bob", 150_000L)
      )
    )

  test("limit: top-2 by salary"):
    val db = seeded
    val rows = db.query[Employee] { e =>
      find(e.name, e.salary).orderBy(e.salary.desc).limit(2)
    }.run
    assertEquals(rows, Vector(("Bob", 150_000L), ("Matt", 120_000L)))

  test("offset: skip first then take"):
    val db = seeded
    val rows = db.query[Employee] { e =>
      find(e.name, e.salary).orderBy(e.salary.asc).offset(1).limit(1)
    }.run
    assertEquals(rows, Vector(("Matt", 120_000L)))

  test("orderBy on aggregation column"):
    val db = seeded
    val rows = db.query[Employee, Order] { (e, o) =>
      find(e.name, sum(o.price))
        .where(e.email === o.buyer)
        .orderBy(sum(o.price).desc)
    }.run
    assertEquals(rows, Vector(("Alice", 90L), ("Bob", 60L), ("Matt", 20L)))

  test("orderBy on a term not in find is a runtime error"):
    val db = seeded
    val ex = intercept[RuntimeException] {
      db.query[Employee] { e =>
        find(e.name).orderBy(e.salary.desc)
      }.run
    }
    assert(ex.getMessage.contains("does not appear in find"))

  test("Query[A]: orderBy ascending"):
    val db = seeded
    val rows = db.where[Employee].orderBy(_.salary).run.map(_.name)
    assertEquals(rows, Vector("Alice", "Matt", "Bob"))

  test("Query[A]: orderByDesc + limit"):
    val db = seeded
    val rows = db.where[Employee].orderByDesc(_.salary).limit(2).run.map(_.name)
    assertEquals(rows, Vector("Bob", "Matt"))

  test("Query[A]: offset + limit"):
    val db = seeded
    val rows = db.where[Employee].orderBy(_.name).offset(1).limit(1).run.map(_.name)
    assertEquals(rows, Vector("Bob"))

  test("not: equality on a single field excludes one row"):
    val db = seeded
    val rows = db.query[Employee] { e =>
      find(e.name) where not(e.email === "m@x.io")
    }.run.toSet
    assertEquals(rows, Set("Alice", "Bob"))

  test("not: anti-join across entities (employees with no orders)"):
    val db = Db()
    db.add(
      Employee("m@x.io", "Matt", Department("ENG", "Engineering"), 120_000L),
      Employee("a@x.io", "Alice", Department("ENG", "Engineering"), 90_000L),
      Employee("b@x.io", "Bob", Department("SAL", "Sales"), 150_000L)
    )
    db.add(Order("m@x.io", "book", 20L))
    val rows = db.query[Employee, Order] { (e, o) =>
      find(e.name) where not(e.email === o.buyer)
    }.run.toSet
    assertEquals(rows, Set("Alice", "Bob"))

  test("not: combine with positive constraint"):
    val db = Db()
    db.add(
      Employee("m@x.io", "Matt", Department("ENG", "Engineering"), 120_000L),
      Employee("a@x.io", "Alice", Department("ENG", "Engineering"), 90_000L),
      Employee("b@x.io", "Bob", Department("SAL", "Sales"), 150_000L)
    )
    db.add(Order("m@x.io", "book", 20L), Order("a@x.io", "lamp", 90L))
    val rows = db.query[Employee, Department, Order] { (e, d, o) =>
      find(e.name) where (
        e.dept === d && d.name === "Engineering" && not(e.email === o.buyer)
      )
    }.run.toSet
    assertEquals(rows, Set.empty[String])

  test("not: department with no employees"):
    val db = Db()
    db.add(Department("ENG", "Engineering"), Department("SAL", "Sales"))
    db.add(Employee("m@x.io", "Matt", Department("ENG", "Engineering"), 120_000L))
    val rows = db.query[Department, Employee] { (d, e) =>
      find(d.name) where not(e.dept === d)
    }.run.toSet
    assertEquals(rows, Set("Sales"))

  test("not: composes with orderBy + limit"):
    val db = seeded
    val rows = db.query[Employee] { e =>
      find(e.name, e.salary)
        .where(not(e.email === "m@x.io"))
        .orderBy(e.salary.desc)
        .limit(1)
    }.run
    assertEquals(rows, Vector(("Bob", 150_000L)))

  test("not: rejected inside ||"):
    val db = seeded
    val ex = intercept[RuntimeException] {
      db.query[Employee] { e =>
        find(e.name) where (e.salary > 100_000L || not(e.email === "m@x.io"))
      }.run
    }
    assert(ex.getMessage.contains("not(...) cannot appear inside"))

  test("not: rejected when nested"):
    val db = seeded
    val ex = intercept[RuntimeException] {
      db.query[Employee] { e =>
        find(e.name) where not(not(e.email === "m@x.io"))
      }.run
    }
    assert(ex.getMessage.contains("nested not"))

  private def seededWindow: Db =
    val db = Db()
    db.add(
      Employee("m@x.io", "Matt", Department("ENG", "Engineering"), 120_000L),
      Employee("a@x.io", "Alice", Department("ENG", "Engineering"), 90_000L),
      Employee("c@x.io", "Carol", Department("ENG", "Engineering"), 90_000L),
      Employee("b@x.io", "Bob", Department("SAL", "Sales"), 150_000L),
      Employee("d@x.io", "Dan", Department("SAL", "Sales"), 80_000L)
    )
    db

  test("window: rowNumber per partition"):
    val db = seededWindow
    val rows = db.query[Employee, Department] { (e, d) =>
      find(d.name, e.name, rowNumber() over partitionBy(d).orderBy(e.salary.desc))
        .where(e.dept === d)
    }.run.toSet
    val byDept = rows.groupBy(_._1).view.mapValues(_.map(_._3).toVector.sorted)
    assertEquals(byDept("Engineering").toList, List(1L, 2L, 3L))
    assertEquals(byDept("Sales").toList, List(1L, 2L))

  test("window: rank ties get same rank, next skips"):
    val db = seededWindow
    val rows = db.query[Employee, Department] { (e, d) =>
      find(e.name, e.salary, rank() over partitionBy(d).orderBy(e.salary.desc))
        .where(e.dept === d && d.name === "Engineering")
    }.run.toSet
    assertEquals(rows.find(_._1 == "Matt").get._3, 1L)
    val ties = rows.filter(_._2 == 90_000L).map(_._3)
    assertEquals(ties, Set(2L))

  test("window: denseRank does not skip after a tie"):
    val db = seededWindow
    val rows = db.query[Employee, Department] { (e, d) =>
      find(e.name, e.salary, denseRank() over partitionBy(d).orderBy(e.salary.asc))
        .where(e.dept === d && d.name === "Engineering")
    }.run.toSet
    val ranksByMatt = rows.find(_._1 == "Matt").get._3
    assertEquals(ranksByMatt, 2L)

  test("window: sum aggregate broadcast over partition"):
    val db = seededWindow
    val rows = db.query[Employee, Department] { (e, d) =>
      find(e.name, sum(e.salary) over partitionBy(d))
        .where(e.dept === d)
    }.run.toMap
    assertEquals(rows("Matt"), 300_000L)
    assertEquals(rows("Alice"), 300_000L)
    assertEquals(rows("Carol"), 300_000L)
    assertEquals(rows("Bob"), 230_000L)
    assertEquals(rows("Dan"), 230_000L)

  test("window: lag returns Some prior value, None at boundary"):
    val db = seededWindow
    val rows = db.query[Employee, Department] { (e, d) =>
      find(e.name, e.salary, lag(e.salary) over partitionBy(d).orderBy(e.salary.asc))
        .where(e.dept === d && d.name === "Sales")
    }.run.toSet
    val danRow = rows.find(_._1 == "Dan").get
    val bobRow = rows.find(_._1 == "Bob").get
    assertEquals(danRow._3, None)
    assertEquals(bobRow._3, Some(80_000L))

  test("window: top-N per partition via rowNumber + orderBy + filter"):
    val db = seededWindow
    val rows = db.query[Employee, Department] { (e, d) =>
      find(d.name, e.name, e.salary, rowNumber() over partitionBy(d).orderBy(e.salary.desc))
        .where(e.dept === d)
        .orderBy(d.name.asc, e.salary.desc)
    }.run
    val tops = rows.filter(_._4 == 1L).map(r => (r._1, r._2))
    assertEquals(tops, Vector(("Engineering", "Matt"), ("Sales", "Bob")))

  test("window: rejected mixed with aggregation in same find"):
    val db = seededWindow
    val ex = intercept[RuntimeException] {
      db.query[Employee, Department] { (e, d) =>
        find(d.name, count(e), rank() over partitionBy(d).orderBy(e.salary.asc))
          .where(e.dept === d)
      }.run
    }
    assert(ex.getMessage.contains("mixes aggregations and window"))
