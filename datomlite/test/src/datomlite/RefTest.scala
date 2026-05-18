package datomlite

class RefTest extends munit.FunSuite:

  case class Tagged(name: String, tags: Set[String])
  object Tagged:
    given Entity[Tagged] = Entity.derived[Tagged].key(_.name)

  test("card-many value: Set[String] round-trips"):
    val db = Db()
    val matt = Tagged("matt", Set("fp", "scala"))
    db.add(matt)
    assertEquals(db.where[Tagged].run, Vector(matt))

  test("card-many value: AVET probe finds entity by one member"):
    val db = Db()
    db.add(Tagged("matt", Set("fp", "scala")))
    db.add(Tagged("ali", Set("ml")))
    val byFp = db.internal.byEqByName[Tagged]("tags", "fp").run
    assertEquals(byFp.map(_.name), Vector("matt"))

  test("card-many value: retract removes all per-value AVET entries"):
    val db = Db()
    val matt = Tagged("matt", Set("fp", "scala"))
    db.add(matt)
    db.retract(matt)
    assertEquals(db.internal.byEqByName[Tagged]("tags", "fp").run, Vector.empty)
    assertEquals(db.internal.byEqByName[Tagged]("tags", "scala").run, Vector.empty)
    assertEquals(db.where[Tagged].run, Vector.empty)

  test("card-many value: typed _.tags.contains(x) probes AVET"):
    val db = Db()
    db.add(Tagged("matt", Set("fp", "scala")))
    db.add(Tagged("ali", Set("ml")))
    val byFp = db.where[Tagged](_.tags.contains("fp")).run
    assertEquals(byFp.map(_.name), Vector("matt"))
    val byMl = db.where[Tagged](_.tags.contains("ml")).run
    assertEquals(byMl.map(_.name), Vector("ali"))
    assertEquals(db.where[Tagged](_.tags.contains("nope")).run, Vector.empty)

  test("card-many value: _.tags(x) sugar probes AVET (Set as Function1)"):
    val db = Db()
    db.add(Tagged("matt", Set("fp")))
    val got = db.where[Tagged](_.tags("fp")).run
    assertEquals(got.map(_.name), Vector("matt"))

  test("card-one String#contains is NOT routed to AVET (would be wrong)"):
    val db = Db()
    db.add(Tagged("matt", Set("fp")))
    db.add(Tagged("ali", Set("ml")))
    val got = db.where[Tagged](_.name.contains("at")).run
    assertEquals(got.map(_.name), Vector("matt"))

  case class Department(code: String, name: String)
  object Department:
    given Entity[Department] = Entity.derived[Department].key(_.code)

  case class Worker(email: String, dept: Department)
  object Worker:
    given Entity[Worker] = Entity.derived[Worker].key(_.email)

  test("card-one ref: nested Department round-trips"):
    val db = Db()
    val eng = Department("ENG", "Engineering")
    val w = Worker("m@x.io", eng)
    db.add(w)
    assertEquals(db.where[Worker].run, Vector(w))
    assertEquals(db.where[Department].run.toSet, Set(eng))

  test("card-one ref: DSL joins across the ref"):
    val db = Db()
    db.add(Worker("m@x.io", Department("ENG", "Engineering")))
    db.add(Worker("a@x.io", Department("SAL", "Sales")))
    val rows = db.query[Worker, Department] { (w, d) =>
      find(w.email, d.name) where (w.dept === d)
    }.run.toSet
    assertEquals(rows, Set(("m@x.io", "Engineering"), ("a@x.io", "Sales")))

  case class Skill(name: String)
  object Skill:
    given Entity[Skill] = Entity.derived[Skill].key(_.name)

  case class Engineer(email: String, skills: Set[Skill])
  object Engineer:
    given Entity[Engineer] = Entity.derived[Engineer].key(_.email)

  test("card-many ref: Set[Skill] round-trips with nested instances"):
    val db = Db()
    val e = Engineer("m@x.io", Set(Skill("scala"), Skill("rust")))
    db.add(e)
    val got = db.where[Engineer].run
    assertEquals(got.size, 1)
    assertEquals(got.head.email, "m@x.io")
    assertEquals(got.head.skills, Set(Skill("scala"), Skill("rust")))
    assertEquals(db.where[Skill].run.map(_.name).toSet, Set("scala", "rust"))

  test("card-many ref: DSL joins to the per-skill rows"):
    val db = Db()
    db.add(Engineer("m@x.io", Set(Skill("scala"), Skill("rust"))))
    db.add(Engineer("a@x.io", Set(Skill("scala"))))
    val rows = db.query[Engineer, Skill] { (en, sk) =>
      find(en.email, sk.name) where (en.skills === sk)
    }.run.toSet
    assertEquals(
      rows,
      Set(
        ("m@x.io", "scala"),
        ("m@x.io", "rust"),
        ("a@x.io", "scala")
      )
    )

  test("ref dedup: same nested Department asserted twice → one row"):
    val db = Db()
    val eng = Department("ENG", "Engineering")
    db.add(Worker("m@x.io", eng))
    db.add(Worker("a@x.io", eng))
    assertEquals(db.where[Department].run.toSet, Set(eng))

  test("ref dedup: two Workers with same dept share one Department row"):
    val db = Db()
    db.add(Worker("m@x.io", Department("ENG", "Engineering")))
    db.add(Worker("a@x.io", Department("ENG", "Engineering")))
    assertEquals(db.where[Department].count, 1L)

  test("ref target without .key fails at derivation time"):
    case class NoKey(x: String)
    given Entity[NoKey] = Entity.derived[NoKey]
    case class Refs(noKey: NoKey)
    val ex = intercept[IllegalArgumentException] {
      Entity.derived[Refs]
    }
    assert(ex.getMessage.contains("needs .key"), s"got: ${ex.getMessage}")

  test("Set[T] ref target without .key fails at derivation time"):
    case class NoKey2(x: String)
    given Entity[NoKey2] = Entity.derived[NoKey2]
    case class SetRefs(noKeys: Set[NoKey2])
    val ex = intercept[IllegalArgumentException] {
      Entity.derived[SetRefs]
    }
    assert(ex.getMessage.contains("needs .key"), s"got: ${ex.getMessage}")

  test("ref-eq: _.dept == someDept resolves the ref then probes AVET"):
    val db = Db()
    val eng = Department("ENG", "Engineering")
    val sal = Department("SAL", "Sales")
    db.add(Worker("m@x.io", eng), Worker("a@x.io", eng), Worker("b@x.io", sal))
    val rows = db.where[Worker](_.dept == eng).run
    assertEquals(rows.map(_.email).toSet, Set("m@x.io", "a@x.io"))

  test("ref-eq: missing ref returns empty without scanning"):
    val db = Db()
    val eng = Department("ENG", "Engineering")
    db.add(Worker("m@x.io", eng))
    val unknown = Department("MKT", "Marketing")
    assertEquals(db.where[Worker](_.dept == unknown).run, Vector.empty)

  test("ref-eq: findOne via ref"):
    val db = Db()
    val eng = Department("ENG", "Engineering")
    db.add(Worker("m@x.io", eng))
    assertEquals(db.where[Worker](_.dept == eng).one.map(_.email), Some("m@x.io"))

  test("ref-eq: where via ref returns a Query usable in for-comp"):
    val db = Db()
    val eng = Department("ENG", "Engineering")
    db.add(Worker("m@x.io", eng), Worker("a@x.io", eng))
    val rows = (for w <- db.where[Worker](_.dept == eng) yield w.email).run.toSet
    assertEquals(rows, Set("m@x.io", "a@x.io"))

  test("ref-walk: _.dept.name == const finds workers via leaf attr"):
    val db = Db()
    db.add(
      Worker("m@x.io", Department("ENG", "Engineering")),
      Worker("a@x.io", Department("ENG", "Engineering")),
      Worker("b@x.io", Department("SAL", "Sales"))
    )
    val rows = db.where[Worker](_.dept.name == "Engineering").run
    assertEquals(rows.map(_.email).toSet, Set("m@x.io", "a@x.io"))

  test("ref-walk: _.dept.code == const probes leaf and joins back"):
    val db = Db()
    db.add(
      Worker("m@x.io", Department("ENG", "Engineering")),
      Worker("b@x.io", Department("SAL", "Sales"))
    )
    val rows = db.where[Worker](_.dept.code == "SAL").run
    assertEquals(rows.map(_.email), Vector("b@x.io"))

  test("ref-walk: no leaf match returns empty"):
    val db = Db()
    db.add(Worker("m@x.io", Department("ENG", "Engineering")))
    assertEquals(db.where[Worker](_.dept.name == "Marketing").run, Vector.empty)

  test("ref-walk: findOne over ref-walk"):
    val db = Db()
    db.add(Worker("m@x.io", Department("ENG", "Engineering")))
    assertEquals(db.where[Worker](_.dept.name == "Engineering").one.map(_.email), Some("m@x.io"))

  test("ref-walk: where over ref-walk for for-comp"):
    val db = Db()
    db.add(
      Worker("m@x.io", Department("ENG", "Engineering")),
      Worker("a@x.io", Department("ENG", "Engineering")),
      Worker("b@x.io", Department("SAL", "Sales"))
    )
    val rows = (for w <- db.where[Worker](_.dept.name == "Engineering") yield w.email).run.toSet
    assertEquals(rows, Set("m@x.io", "a@x.io"))

  test("ref-walk: deduped across multiple matching parent eids"):
    val db = Db()
    db.add(
      Worker("m@x.io", Department("ENG", "Engineering")),
      Worker("a@x.io", Department("ENG", "Engineering"))
    )
    val rows = db.where[Worker](_.dept.name == "Engineering").run
    assertEquals(rows.size, 2)
