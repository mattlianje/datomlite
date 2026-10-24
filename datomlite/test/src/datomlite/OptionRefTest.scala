package datomlite

class OptionRefTest extends munit.FunSuite:

  case class Team(code: String, name: String)
  object Team:
    given Entity[Team] = Entity.derived[Team].key(_.code)

  case class Player(email: String, team: Option[Team])
  object Player:
    given Entity[Player] = Entity.derived[Player].key(_.email)

  case class Node(id: Int, parent: Option[Node])
  object Node:
    given Entity[Node] = Entity.derived[Node].key(_.id)

  private val red = Team("R", "Red")
  private val blue = Team("B", "Blue")

  test("Option[Ref]: None writes no datom, Some writes the target eid"):
    val db = Db()
    db.add(Player("a@x.io", None), Player("b@x.io", Some(red)))
    val refs = db.log.filter(_.a == "Player/team").map(_.v)
    val redEid = db.internal.eidOf(red)
    assertEquals(refs, redEid.toVector)

  test("Option[Ref]: round-trips both cases"):
    val db = Db()
    db.add(Player("a@x.io", None), Player("b@x.io", Some(red)))
    assertEquals(
      db.where[Player].run.toSet,
      Set(Player("a@x.io", None), Player("b@x.io", Some(red)))
    )

  test("Option[Ref]: the target is shared, not duplicated"):
    val db = Db()
    db.add(Player("a@x.io", Some(red)), Player("b@x.io", Some(red)))
    assertEquals(db.where[Team].count, 1L)

  test("Option[Ref]: where .contains probes by the target's eid"):
    val db = Db()
    db.add(Player("a@x.io", Some(red)), Player("b@x.io", Some(blue)), Player("c@x.io", None))
    assertEquals(db.where[Player](_.team.contains(blue)).run.map(_.email), Vector("b@x.io"))
    assertEquals(db.where[Player](_.team.isEmpty).run.map(_.email), Vector("c@x.io"))

  test("Option[Ref]: typed query joins through it"):
    val db = Db()
    db.add(Player("a@x.io", Some(red)), Player("b@x.io", Some(blue)), Player("c@x.io", None))
    val rows = db.query[Player, Team] { (p, t) =>
      find(p.email, t.name) where (p.team === t)
    }.run
    assertEquals(rows.toSet, Set("a@x.io" -> "Red", "b@x.io" -> "Blue"))

  test("Option[Ref]: pull gives Option[Pull[R]]"):
    val db = Db()
    db.add(Player("a@x.io", Some(red)), Player("c@x.io", None))
    val a = db.internal.eidOf(Player("a@x.io", Some(red))).get
    val c = db.internal.eidOf(Player("c@x.io", None)).get
    assertEquals(db.pullByEid[Player](a)(_.team.map(_(_.name))), Some(Some("Red")))
    assertEquals(db.pullByEid[Player](c)(_.team.map(_.value)), Some(None))

  test("Option[Ref]: reverse pull through an optional ref"):
    val db = Db()
    db.add(Player("a@x.io", Some(red)), Player("b@x.io", Some(red)), Player("c@x.io", None))
    val r = db.internal.eidOf(red).get
    val emails = db.pullByEid[Team](r)(_.reverse[Player](_.team).map(_.value.email))
    assertEquals(emails.map(_.toSet), Some(Set("a@x.io", "b@x.io")))

  test("Option[Ref]: upsert swaps Some to None and back"):
    val db = Db()
    db.add(Player("a@x.io", Some(red)))
    db.upsert(Player("a@x.io", None))
    assertEquals(db.where[Player].run, Vector(Player("a@x.io", None)))
    db.upsert(Player("a@x.io", Some(blue)))
    assertEquals(db.where[Player].run, Vector(Player("a@x.io", Some(blue))))

  test("Option[Ref]: self-referencing tree"):
    val db = Db()
    val root = Node(1, None)
    val child = Node(2, Some(root))
    val grandchild = Node(3, Some(child))
    db.add(grandchild)
    assertEquals(db.where[Node].run.toSet, Set(root, child, grandchild))
    assertEquals(db.where[Node](_.parent.contains(root)).run, Vector(child))
