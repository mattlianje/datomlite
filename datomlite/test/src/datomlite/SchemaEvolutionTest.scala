package datomlite

/** Each test writes with one version of a case class and reads back with a later one. Both versions
  * share the entity name, as they would across two deploys of the same app
  */
class SchemaEvolutionTest extends munit.FunSuite:

  final class MemStorage extends Storage:
    var datoms = Vector.empty[Datom]
    def load(): Vector[Datom] = datoms
    def append(ds: Vector[Datom]): Unit = datoms ++= ds

  object v1:
    case class Person(email: String, name: String, age: Int)
    given Entity[Person] = Entity.derived[Person].key(_.email).named("Person")

  object v2:
    // gained `active` (defaulted) and `nick` (optional), dropped nothing
    case class Person(
        email: String,
        name: String,
        age: Int,
        active: Boolean = true,
        nick: Option[String]
    )
    given Entity[Person] = Entity.derived[Person].key(_.email).named("Person")

  object v3:
    // dropped `age`
    case class Person(email: String, name: String)
    given Entity[Person] = Entity.derived[Person].key(_.email).named("Person")

  object v4:
    // `age` is now a Long
    case class Person(email: String, name: String, age: Long)
    given Entity[Person] = Entity.derived[Person].key(_.email).named("Person")

  object v5:
    // gained a required field with no default
    case class Person(email: String, name: String, age: Int, country: String)
    given Entity[Person] = Entity.derived[Person].key(_.email).named("Person")

  object renamed:
    // `name` renamed to `fullName`, entity renamed to Member
    case class Member(email: String, fullName: String, age: Int)
    given Entity[Member] = Entity.derived[Member].key(_.email)

  private def seeded(): MemStorage =
    val s = MemStorage()
    import v1.given
    Db(s).add(v1.Person("a@x.io", "Ann", 30), v1.Person("b@x.io", "Bob", 40))
    s

  test("added field: default fills older rows, Option reads as None"):
    import v2.given
    val db = Db(seeded())
    assertEquals(
      db.where[v2.Person].run.toSet,
      Set(v2.Person("a@x.io", "Ann", 30, true, None), v2.Person("b@x.io", "Bob", 40, true, None))
    )

  test("added field: queries and probes see the default on older rows"):
    import v2.given
    val db = Db(seeded())
    db.add(v2.Person("c@x.io", "Cy", 20, active = false, nick = None))
    assertEquals(
      db.where[v2.Person](_.active == true).run.map(_.email).toSet,
      Set("a@x.io", "b@x.io")
    )
    assertEquals(db.where[v2.Person](_.active == false).run.map(_.email), Vector("c@x.io"))
    val active = db.query[v2.Person] { p =>
      find(p.email) where (p.active === true)
    }.run
    assertEquals(active.toSet, Set("a@x.io", "b@x.io"))
    val all = db.query[v2.Person](p => find(p.email, p.active)).run
    assertEquals(all.toSet, Set("a@x.io" -> true, "b@x.io" -> true, "c@x.io" -> false))
    val eid = db.internal.eidOf(v2.Person("a@x.io", "Ann", 30, true, None)).get
    assertEquals(db.pullByEid[v2.Person](eid)(_.active), Some(true))

  test("added field: re-adding an older row as-is is a no-op"):
    import v2.given
    val storage = seeded()
    val db = Db(storage)
    val before = storage.datoms.size
    db.add(v2.Person("a@x.io", "Ann", 30, true, None))
    assertEquals(storage.datoms.size, before)

  test("added field: retract and upsert work on older rows"):
    import v2.given
    val db = Db(seeded())
    db.upsert(v2.Person("a@x.io", "Ann", 31, false, Some("annie")))
    db.retract(v2.Person("b@x.io", "Bob", 40, true, None))
    assertEquals(
      db.where[v2.Person].run,
      Vector(v2.Person("a@x.io", "Ann", 31, false, Some("annie")))
    )

  test("added field without a default: MissingField names the entity and field"):
    import v5.given
    val db = Db(seeded())
    val err = intercept[MissingField](db.where[v5.Person].run)
    assertEquals((err.entityName, err.field), ("Person", "country"))
    assert(err.getMessage.contains("give it a default"))

  test("removed field: older datoms are ignored on read"):
    import v3.given
    val db = Db(seeded())
    assertEquals(
      db.where[v3.Person].run.toSet,
      Set(v3.Person("a@x.io", "Ann"), v3.Person("b@x.io", "Bob"))
    )

  test("removed field: retract clears the stale datoms too"):
    import v3.given
    val storage = seeded()
    Db(storage).retract(v3.Person("a@x.io", "Ann"))
    val db = Db(storage)
    assertEquals(db.where[v3.Person].run, Vector(v3.Person("b@x.io", "Bob")))
    assert(db.pretty.linesIterator.forall(!_.contains("Ann")))
    assert(!db.pretty.contains("#0"))

  test("widened field: Int datoms read as Long"):
    import v4.given
    val db = Db(seeded())
    assertEquals(db.where[v4.Person](_.age == 30L).run, Vector(v4.Person("a@x.io", "Ann", 30L)))
    assertEquals(db.where[v4.Person].run.map(_.age).toSet, Set(30L, 40L))

  test("migration: renameEntity + renameField"):
    import renamed.given
    val storage = seeded().migrate(
      Migration.renameEntity[renamed.Member]("Person"),
      Migration.renameField[renamed.Member](_.fullName, from = "name")
    )
    val db = Db(storage)
    assertEquals(
      db.where[renamed.Member].run.toSet,
      Set(renamed.Member("a@x.io", "Ann", 30), renamed.Member("b@x.io", "Bob", 40))
    )
    db.add(renamed.Member("c@x.io", "Cy", 20))
    assertEquals(Db(storage).where[renamed.Member].count, 3L)

  test("migration: removeField drops the datoms from the log"):
    import v3.given
    val db = Db(seeded().migrate(Migration.removeField[v3.Person]("age")))
    assert(db.log.forall(_.a != "Person/age"))

  test("migration: raw rewrite"):
    import v1.given
    val bump = Migration { d =>
      Some(if d.a == "Person/age" then d.copy(v = d.v.asInstanceOf[Int] + 1) else d)
    }
    val db = Db(seeded().migrate(bump))
    assertEquals(db.where[v1.Person].run.map(_.age).toSet, Set(31, 41))

  test("nested value: added field with default reads older values"):
    case class AddrV1(city: String) derives Codec
    case class AddrV2(city: String, zip: String = "?") derives Codec
    val stored = Codec[AddrV1].encode(AddrV1("Paris"))
    assertEquals(Codec[AddrV2].decode(stored), AddrV2("Paris", "?"))
    val err = intercept[MissingField](Codec[(String, Int)].decode(Vector("_1", "x")))
    assertEquals(err.field, "_2")
