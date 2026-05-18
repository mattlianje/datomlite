package datomlite

class NamedTest extends munit.FunSuite:

  case class Person(email: String, name: String)
  object Person:
    given Entity[Person] = Entity.derived[Person].key(_.email).named("People")

  test("named: storage attribute keys use the override"):
    val db = Db()
    db.add(Person("m@x.io", "Matt"))
    assertEquals(db.log.map(_.a).toSet, Set("People/email", "People/name"))

  test("named: AVET-routed equality predicate still hits the right rows"):
    val db = Db()
    db.add(Person("m@x.io", "Matt"), Person("a@x.io", "Alice"))
    assertEquals(db.where[Person](_.email == "m@x.io").one, Some(Person("m@x.io", "Matt")))

  test("named: pull descends through the renamed attribute"):
    val db = Db()
    val rep = db.add(Person("m@x.io", "Matt"))
    val eid = rep.addedEids.head
    val pulled = db.pullByEid[Person](eid)(p => (p.email, p.name))
    assertEquals(pulled, Some(("m@x.io", "Matt")))

  test("named: UniqueViolation reports the override name"):
    val db = Db()
    db.add(Person("m@x.io", "Matt"))
    val ex = intercept[UniqueViolation](db.add(Person("m@x.io", "Matty")))
    assertEquals(ex.entityName, "People")

  case class Owner(handle: String)
  object Owner:
    given Entity[Owner] = Entity.derived[Owner].key(_.handle).named("Humans")

  case class Pet(name: String, owner: Owner)
  object Pet:
    given Entity[Pet] = Entity.derived[Pet].key(_.name)

  test("named: ref-walk equality predicate routes through the renamed parent"):
    val db = Db()
    db.add(Owner("matt"), Pet("Rex", Owner("matt")))
    assertEquals(db.where[Pet](_.owner.handle == "matt").one.map(_.name), Some("Rex"))
    assert(db.log.exists(_.a == "Humans/handle"))
