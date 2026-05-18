package datomlite

class PullTest extends munit.FunSuite:

  case class Department(code: String, name: String)
  object Department:
    given Entity[Department] = Entity.derived[Department].key(_.code)

  case class Skill(name: String)
  object Skill:
    given Entity[Skill] = Entity.derived[Skill].key(_.name)

  case class Employee(
      email: String,
      name: String,
      dept: Department,
      skills: Set[Skill],
      tags: Set[String]
  )
  object Employee:
    given Entity[Employee] = Entity.derived[Employee].key(_.email)

  test("TxReport carries addedEids in the order assertions were issued"):
    val db = Db()
    val rep = db.add(
      Employee("m@x.io", "Matt", Department("ENG", "Engineering"), Set.empty, Set("fp")),
      Employee("a@x.io", "Alice", Department("ENG", "Engineering"), Set.empty, Set("ml"))
    )
    assertEquals(rep.addedEids.size, 2)
    assertEquals(
      rep.addedEids.flatMap(eid => db.byEid[Employee](eid).map(_.email)),
      Vector("m@x.io", "a@x.io")
    )

  test("TxReport.addedEids covers only top-level assertions, not nested refs"):
    val db = Db()
    val rep = db.add(
      Employee("m@x.io", "Matt", Department("ENG", "Engineering"), Set.empty, Set.empty)
    )
    assertEquals(rep.addedEids.size, 1)
    assertEquals(db.byEid[Employee](rep.addedEids.head).map(_.email), Some("m@x.io"))

  test("pull by eid: scalar fields"):
    val db = Db()
    val rep = db.add(
      Employee("m@x.io", "Matt", Department("ENG", "Engineering"), Set.empty, Set.empty)
    )
    val eid = rep.addedEids.head
    assertEquals(db.pullByEid[Employee](eid)(_.name), Some("Matt"))
    assertEquals(
      db.pullByEid[Employee](eid)(e => (e.email, e.name)),
      Some(("m@x.io", "Matt"))
    )

  test("pull by eid: ref descent via apply"):
    val db = Db()
    val rep = db.add(
      Employee("m@x.io", "Matt", Department("ENG", "Engineering"), Set.empty, Set.empty)
    )
    val eid = rep.addedEids.head
    val out = db.pullByEid[Employee](eid)(e => (e.name, e.dept.apply(_.name)))
    assertEquals(out, Some(("Matt", "Engineering")))

  test("pull by eid: nested ref tuple"):
    val db = Db()
    val rep = db.add(
      Employee("m@x.io", "Matt", Department("ENG", "Engineering"), Set.empty, Set.empty)
    )
    val eid = rep.addedEids.head
    val out =
      db.pullByEid[Employee](eid)(e => (e.email, e.dept.apply(d => (d.code, d.name))))
    assertEquals(out, Some(("m@x.io", ("ENG", "Engineering"))))

  test("pull by eid: bare ref returns Pull[T], use .value to materialize"):
    val db = Db()
    val rep = db.add(
      Employee("m@x.io", "Matt", Department("ENG", "Engineering"), Set.empty, Set.empty)
    )
    val eid = rep.addedEids.head
    val dept = db.pullByEid[Employee](eid)(_.dept.value)
    assertEquals(dept, Some(Department("ENG", "Engineering")))

  test("pull by eid: card-many ref, sub-pull fans out into Vector"):
    val db = Db()
    val rep = db.add(Employee(
      "m@x.io",
      "Matt",
      Department("ENG", "Engineering"),
      Set(Skill("scala"), Skill("rust")),
      Set.empty
    ))
    val eid = rep.addedEids.head
    val names = db.pullByEid[Employee](eid)(_.skills.map(_.name)).getOrElse(Vector.empty)
    assertEquals(names.toSet, Set("scala", "rust"))

  test("pull by eid: card-many ref bare .value reconstructs full Vector"):
    val db = Db()
    val rep = db.add(Employee(
      "m@x.io",
      "Matt",
      Department("ENG", "Engineering"),
      Set(Skill("scala")),
      Set.empty
    ))
    val eid = rep.addedEids.head
    val skills = db.pullByEid[Employee](eid)(_.skills.value).getOrElse(Vector.empty)
    assertEquals(skills, Vector(Skill("scala")))

  test("pull by eid: card-many value (Set[String])"):
    val db = Db()
    val rep = db.add(Employee(
      "m@x.io",
      "Matt",
      Department("ENG", "Engineering"),
      Set.empty,
      Set("fp", "scala")
    ))
    val eid = rep.addedEids.head
    val tags = db.pullByEid[Employee](eid)(_.tags).getOrElse(Set.empty)
    assertEquals(tags, Set("fp", "scala"))

  test("pull by eid: missing eid returns None"):
    val db = Db()
    db.add(Employee("m@x.io", "Matt", Department("ENG", "Engineering"), Set.empty, Set.empty))
    assertEquals(db.pullByEid[Employee](999_999L)(_.name), None)

  test("pull by eid: wrong class returns None"):
    val db = Db()
    val rep = db.add(Department("ENG", "Engineering"))
    val deptEid = rep.addedEids.head
    assertEquals(db.pullByEid[Employee](deptEid)(_.name), None)

  test("pull by eid: top-level .value materializes whole entity"):
    val db = Db()
    val matt = Employee("m@x.io", "Matt", Department("ENG", "Engineering"), Set.empty, Set.empty)
    val rep = db.add(matt)
    val out = db.pullByEid[Employee](rep.addedEids.head)(_.value)
    assertEquals(out, Some(matt))

  test("pullOne by predicate: equality routes through AVET, returns Some"):
    val db = Db()
    db.add(
      Employee("m@x.io", "Matt", Department("ENG", "Engineering"), Set.empty, Set.empty),
      Employee("a@x.io", "Alice", Department("ENG", "Engineering"), Set.empty, Set.empty)
    )
    val out = db.where[Employee](_.email == "m@x.io").pullOne(e => (e.name, e.dept.apply(_.name)))
    assertEquals(out, Some(("Matt", "Engineering")))

  test("pullOne by predicate: no match returns None"):
    val db = Db()
    db.add(Employee("m@x.io", "Matt", Department("ENG", "Engineering"), Set.empty, Set.empty))
    assertEquals(db.where[Employee](_.email == "missing@x.io").pullOne(_.name), None)

  test("pullAll: equality predicate, multi-row"):
    val db = Db()
    db.add(
      Employee("m@x.io", "Matt", Department("ENG", "Engineering"), Set.empty, Set.empty),
      Employee("a@x.io", "Alice", Department("ENG", "Engineering"), Set.empty, Set.empty),
      Employee("b@x.io", "Bob", Department("SAL", "Sales"), Set.empty, Set.empty)
    )
    val rows = db.where[Employee](_.dept.name == "Engineering").pull(e => (e.email, e.name)).toSet
    assertEquals(rows, Set(("m@x.io", "Matt"), ("a@x.io", "Alice")))

  test("pullAll: scan-fallback predicate (non-equality) still works"):
    val db = Db()
    db.add(
      Employee("m@x.io", "Matt", Department("ENG", "Engineering"), Set.empty, Set.empty),
      Employee("a@x.io", "Alice", Department("ENG", "Engineering"), Set.empty, Set.empty),
      Employee("b@x.io", "Bob", Department("SAL", "Sales"), Set.empty, Set.empty)
    )
    val rows = db.where[Employee](_.name.startsWith("A")).pull(_.email).toSet
    assertEquals(rows, Set("a@x.io"))

  test("pull ctx is snapped at call time; later transactions don't shift the view"):
    val db = Db()
    val rep = db.add(
      Employee("m@x.io", "Matt", Department("ENG", "Engineering"), Set.empty, Set.empty)
    )
    val eid = rep.addedEids.head
    val pulled = db.pullByEid[Employee](eid) { e =>
      val before = e.name
      db.retractWhere[Employee](_.email == "m@x.io")
      val after = e.name
      (before, after)
    }
    assertEquals(pulled, Some(("Matt", "Matt")))

  test("reverse: card-one back-link gives MultiPull of source entities"):
    val db = Db()
    db.add(
      Employee("m@x.io", "Matt", Department("ENG", "Engineering"), Set.empty, Set.empty),
      Employee("a@x.io", "Alice", Department("ENG", "Engineering"), Set.empty, Set.empty),
      Employee("b@x.io", "Bob", Department("SAL", "Sales"), Set.empty, Set.empty)
    )
    val deptEid = db.internal.eidsByEqByName[Department]("code", "ENG").head
    val emails = db.pullByEid[Department](deptEid) { d =>
      d.reverse[Employee](_.dept).map(_.email)
    }.getOrElse(Vector.empty)
    assertEquals(emails.toSet, Set("m@x.io", "a@x.io"))

  test("reverse: empty when nothing points back"):
    val db = Db()
    db.add(Department("HR", "People"))
    db.add(Employee("m@x.io", "Matt", Department("ENG", "Engineering"), Set.empty, Set.empty))
    val hrEid = db.internal.eidsByEqByName[Department]("code", "HR").head
    val emails = db.pullByEid[Department](hrEid) { d =>
      d.reverse[Employee](_.dept).map(_.email)
    }.getOrElse(Vector.empty)
    assertEquals(emails, Vector.empty[String])

  test("reverse: bare reverse without sub-pull exposes Pulls (use .value to materialize)"):
    val db = Db()
    db.add(
      Employee("m@x.io", "Matt", Department("ENG", "Engineering"), Set.empty, Set.empty),
      Employee("a@x.io", "Alice", Department("ENG", "Engineering"), Set.empty, Set.empty)
    )
    val deptEid = db.internal.eidsByEqByName[Department]("code", "ENG").head
    val emps = db.pullByEid[Department](deptEid) { d =>
      d.reverse[Employee](_.dept).value
    }.getOrElse(Vector.empty)
    assertEquals(emps.map(_.email).toSet, Set("m@x.io", "a@x.io"))

  test("reverse: card-many back-link (Set[Skill] as field) returns the holders"):
    val db = Db()
    db.add(
      Employee("m@x.io", "Matt", Department("ENG", "Engineering"), Set(Skill("scala")), Set.empty),
      Employee("a@x.io", "Alice", Department("ENG", "Engineering"), Set(Skill("scala")), Set.empty),
      Employee("b@x.io", "Bob", Department("SAL", "Sales"), Set.empty, Set.empty)
    )
    val skillEid = db.internal.eidsByEqByName[Skill]("name", "scala").head
    val holders = db.pullByEid[Skill](skillEid) { s =>
      s.reverse[Employee](_.skills).map(_.email)
    }.getOrElse(Vector.empty)
    assertEquals(holders.toSet, Set("m@x.io", "a@x.io"))

  test("reverse: nested sub-pull through reverse(...).map traverses further"):
    val db = Db()
    db.add(
      Employee("m@x.io", "Matt", Department("ENG", "Engineering"), Set(Skill("scala")), Set.empty),
      Employee("a@x.io", "Alice", Department("ENG", "Engineering"), Set(Skill("rust")), Set.empty)
    )
    val deptEid = db.internal.eidsByEqByName[Department]("code", "ENG").head
    val pairs = db.pullByEid[Department](deptEid) { d =>
      d.reverse[Employee](_.dept).map(e => (e.email, e.name))
    }.getOrElse(Vector.empty)
    assertEquals(pairs.toSet, Set(("m@x.io", "Matt"), ("a@x.io", "Alice")))

  test("reverse: nested tree of dept -> employees -> skills"):
    val db = Db()
    db.add(
      Employee(
        "m@x.io",
        "Matt",
        Department("ENG", "Engineering"),
        Set(Skill("scala"), Skill("rust")),
        Set.empty
      ),
      Employee("a@x.io", "Alice", Department("ENG", "Engineering"), Set(Skill("scala")), Set.empty),
      Employee("b@x.io", "Bob", Department("SAL", "Sales"), Set.empty, Set.empty)
    )
    val tree = db.where[Department](_.code == "ENG").pullOne { d =>
      (d.name, d.reverse[Employee](_.dept).map(e => (e.name, e.skills.map(_.name).toSet)))
    }
    assertEquals(
      tree.map { case (deptName, emps) => (deptName, emps.map { case (n, s) => (n, s) }.toSet) },
      Some((
        "Engineering",
        Set(
          ("Matt", Set("scala", "rust")),
          ("Alice", Set("scala"))
        )
      ))
    )

  test("reverse: keeps employees with empty skills; flat query drops them"):
    val db = Db()
    db.add(
      Employee("m@x.io", "Matt", Department("ENG", "Engineering"), Set(Skill("scala")), Set.empty),
      Employee("c@x.io", "Carol", Department("ENG", "Engineering"), Set.empty, Set.empty)
    )

    val pulled = db.where[Department](_.code == "ENG").pullOne { d =>
      d.reverse[Employee](_.dept).map(e => (e.name, e.skills.map(_.name).toSet))
    }.getOrElse(Vector.empty).toSet
    assertEquals(pulled, Set(("Matt", Set("scala")), ("Carol", Set.empty[String])))

    val triples = db.query[Department, Employee, Skill] { (d, e, s) =>
      find(e.name, s.name) where (d.code === "ENG" && e.dept === d && e.skills === s)
    }.run.map(_._1).toSet
    assertEquals(triples, Set("Matt"))

  test("reverse: wrong-target field is a compile error"):
    val errors = compileErrors("""
      import datomlite.*
      val db = Db()
      val skillEid = 0L
      // Skill has no field that targets Department, so this must not compile.
      db.pullByEid[PullTest.this.Department](skillEid) { d =>
        d.reverse[PullTest.this.Skill](_.name)
      }
    """)
    assert(
      errors.contains("does not target") || errors.contains("not a field"),
      s"expected wrong-target reverse rejection, got:\n$errors"
    )

  test("reverse: typo in field name is a compile error"):
    val errors = compileErrors("""
      import datomlite.*
      val db = Db()
      val deptEid = 0L
      db.pullByEid[PullTest.this.Department](deptEid) { d =>
        d.reverse[PullTest.this.Employee](_.dpet)
      }
    """)
    assert(
      errors.contains("dpet") && errors.contains("not a member"),
      s"expected accessor-not-found error, got:\n$errors"
    )
