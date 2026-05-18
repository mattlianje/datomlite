package datomlite

class AnnotationDeriveTest extends munit.FunSuite:

  case class User(@key email: String, @unique handle: String, name: String) derives Entity

  test("@key field powers upsert match"):
    val db = Db()
    db.add(User("m@x.io", "matt", "Matt"))
    db.upsert(User("m@x.io", "matt", "Matty"))
    assertEquals(db.where[User].run, Vector(User("m@x.io", "matt", "Matty")))

  test("@unique field is enforced"):
    val db = Db()
    db.add(User("a@x.io", "shared", "Alice"))
    intercept[Exception] {
      db.add(User("b@x.io", "shared", "Bob"))
    }

  case class Doc(@key slug: String, title: String)

  test("annotation form composes with chained .unique"):
    given Entity[Doc] = Entity.derived[Doc].unique(_.title)
    val db = Db()
    db.add(Doc("a", "T1"))
    db.upsert(Doc("a", "T2"))
    assertEquals(db.where[Doc].run, Vector(Doc("a", "T2")))
    intercept[Exception] {
      db.add(Doc("b", "T2"))
    }

  case class Tag(name: String) derives Entity

  test("no annotations leaves keyOf empty"):
    val e = summon[Entity[Tag]]
    assertEquals(e.keyOf, None)
    assertEquals(e.uniques, Nil)
