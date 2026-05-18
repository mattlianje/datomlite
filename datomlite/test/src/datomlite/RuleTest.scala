package datomlite

class RuleTest extends munit.FunSuite:

  case class Node(id: String, children: Set[Node])
  object Node:
    given Entity[Node] = Entity.derived[Node].key(_.id)

  // Tree:
  //   root --> a, b
  //   a    --> c
  //   c    --> d
  //   b    --> (none)
  private def treeDb: Db =
    val d = Node("d", Set.empty)
    val c = Node("c", Set(d))
    val b = Node("b", Set.empty)
    val a = Node("a", Set(c))
    val root = Node("root", Set(a, b))
    val db = Db()
    db.add(root)
    db

  test("reaches: forward closure with src bound, dst free"):
    val db = treeDb
    val reachable: Rule[Node, Node] = Rule.reaches[Node](_.children)
    val rows = db.query[Node, Node] { (s, t) =>
      find(t.id) where (s.id === "root" && reachable(s, t))
    }.run.toSet
    assertEquals(rows, Set("a", "b", "c", "d"))

  test("reaches: forward closure from a non-root node"):
    val db = treeDb
    val reachable = Rule.reaches[Node](_.children)
    val rows = db.query[Node, Node] { (s, t) =>
      find(t.id) where (s.id === "a" && reachable(s, t))
    }.run.toSet
    assertEquals(rows, Set("c", "d"))

  test("reaches: leaf has empty closure"):
    val db = treeDb
    val reachable = Rule.reaches[Node](_.children)
    val rows = db.query[Node, Node] { (s, t) =>
      find(t.id) where (s.id === "d" && reachable(s, t))
    }.run.toSet
    assertEquals(rows, Set.empty[String])

  test("reaches: reverse closure with dst bound, src free"):
    val db = treeDb
    val reachable = Rule.reaches[Node](_.children)
    val rows = db.query[Node, Node] { (s, t) =>
      find(s.id) where (t.id === "d" && reachable(s, t))
    }.run.toSet
    // Every ancestor of d.
    assertEquals(rows, Set("a", "c", "root"))

  test("reaches: pair check with both sides bound"):
    val db = treeDb
    val reachable = Rule.reaches[Node](_.children)
    val hit = db.query[Node, Node] { (s, t) =>
      find(s.id, t.id) where (s.id === "root" && t.id === "d" && reachable(s, t))
    }.run
    assertEquals(hit, Vector(("root", "d")))

    val miss = db.query[Node, Node] { (s, t) =>
      find(s.id, t.id) where (s.id === "b" && t.id === "d" && reachable(s, t))
    }.run
    assertEquals(miss, Vector.empty[(String, String)])

  test("reaches: full enumeration when neither side is bound"):
    val db = treeDb
    val reachable = Rule.reaches[Node](_.children)
    val pairs = db.query[Node, Node] { (s, t) =>
      find(s.id, t.id) where reachable(s, t)
    }.run.toSet
    val expected = Set(
      ("root", "a"),
      ("root", "b"),
      ("root", "c"),
      ("root", "d"),
      ("a", "c"),
      ("a", "d"),
      ("c", "d")
    )
    assertEquals(pairs, expected)

  test("reaches: composes with orderBy and limit"):
    val db = treeDb
    val reachable = Rule.reaches[Node](_.children)
    val rows = db.query[Node, Node] { (s, t) =>
      find(t.id)
        .where(s.id === "root" && reachable(s, t))
        .orderBy(t.id.asc)
        .limit(2)
    }.run
    assertEquals(rows, Vector("a", "b"))

  test("reaches: composes with negation (descendants of root not equal to a)"):
    val db = treeDb
    val reachable = Rule.reaches[Node](_.children)
    val rows = db.query[Node, Node] { (s, t) =>
      find(t.id) where (s.id === "root" && reachable(s, t) && not(t.id === "a"))
    }.run.toSet
    assertEquals(rows, Set("b", "c", "d"))

  test("Rule.reaches: non-self-ref field is a compile error"):
    val errors = compileErrors("""
      import datomlite.*
      // Node's `id` is a String, not a self-ref, so this must not compile.
      Rule.reaches[RuleTest.this.Node](_.id)
    """)
    assert(
      errors.contains("self-ref"),
      s"expected self-ref rejection, got:\n$errors"
    )

  test("Rule.reaches: typo in field name is a compile error"):
    val errors = compileErrors("""
      import datomlite.*
      Rule.reaches[RuleTest.this.Node](_.childen)
    """)
    assert(
      errors.contains("childen") && errors.contains("not a member"),
      s"expected accessor-not-found error, got:\n$errors"
    )

  case class Off(name: String, boss: Option[String])
  object Off:
    given Entity[Off] = Entity.derived[Off].key(_.name)

  private def flatOfficeDb: Db =
    val db = Db()
    db.add(Off("Napoleon", None))
    db.add(Off("Davout", Some("Napoleon")))
    db.add(Off("Ney", Some("Napoleon")))
    db.add(Off("Murat", Some("Napoleon")))
    db.add(Off("Friant", Some("Davout")))
    db.add(Off("Gudin", Some("Davout")))
    db.add(Off("Marchand", Some("Ney")))
    db.add(Off("Lasalle", Some("Murat")))
    db

  private def ancestorRule: Rule[Off, Off] =
    Rule.recursive[Off, Off] { self =>
      Seq(
        (anc, desc) => desc.boss === anc.name,
        (anc, desc) =>
          exists[Off] { mid =>
            desc.boss === mid.name && self(anc, mid)
          }
      )
    }

  test("recursive: descendants of the root via ancestor(root, _)"):
    val db = flatOfficeDb
    val ancestor = ancestorRule
    val rows = db.query[Off, Off] { (anc, desc) =>
      find(desc.name) where (anc.name === "Napoleon" && ancestor(anc, desc))
    }.run.toSet
    assertEquals(
      rows,
      Set("Davout", "Ney", "Murat", "Friant", "Gudin", "Marchand", "Lasalle")
    )

  test("recursive: ancestors of a leaf via ancestor(_, leaf)"):
    val db = flatOfficeDb
    val ancestor = ancestorRule
    val rows = db.query[Off, Off] { (anc, desc) =>
      find(anc.name) where (desc.name === "Friant" && ancestor(anc, desc))
    }.run.toSet
    assertEquals(rows, Set("Davout", "Napoleon"))

  test("recursive: pair check with both sides bound"):
    val db = flatOfficeDb
    val ancestor = ancestorRule
    val hit = db.query[Off, Off] { (anc, desc) =>
      find(anc.name, desc.name)
        .where(anc.name === "Napoleon" && desc.name === "Lasalle" && ancestor(anc, desc))
    }.run
    assertEquals(hit, Vector(("Napoleon", "Lasalle")))

    val miss = db.query[Off, Off] { (anc, desc) =>
      find(anc.name, desc.name)
        .where(anc.name === "Davout" && desc.name === "Lasalle" && ancestor(anc, desc))
    }.run
    assertEquals(miss, Vector.empty[(String, String)])

  test("recursive: enumerate every (ancestor, descendant) pair"):
    val db = flatOfficeDb
    val ancestor = ancestorRule
    val pairs = db.query[Off, Off] { (anc, desc) =>
      find(anc.name, desc.name) where ancestor(anc, desc)
    }.run.toSet
    val expected = Set(
      ("Napoleon", "Davout"),
      ("Napoleon", "Ney"),
      ("Napoleon", "Murat"),
      ("Napoleon", "Friant"),
      ("Napoleon", "Gudin"),
      ("Napoleon", "Marchand"),
      ("Napoleon", "Lasalle"),
      ("Davout", "Friant"),
      ("Davout", "Gudin"),
      ("Ney", "Marchand"),
      ("Murat", "Lasalle")
    )
    assertEquals(pairs, expected)

  test("recursive: composes with orderBy + limit"):
    val db = flatOfficeDb
    val ancestor = ancestorRule
    val rows = db.query[Off, Off] { (anc, desc) =>
      find(desc.name)
        .where(anc.name === "Napoleon" && ancestor(anc, desc))
        .orderBy(desc.name.asc)
        .limit(3)
    }.run
    assertEquals(rows, Vector("Davout", "Friant", "Gudin"))

  test("recursive: composes with count aggregation per ancestor"):
    val db = flatOfficeDb
    val ancestor = ancestorRule
    val counts = db.query[Off, Off] { (anc, desc) =>
      find(anc.name, count(desc.name)) where ancestor(anc, desc)
    }.run.toMap
    assertEquals(counts("Napoleon"), 7L)
    assertEquals(counts("Davout"), 2L)
    assertEquals(counts("Ney"), 1L)
    assertEquals(counts("Murat"), 1L)

  case class Doc(slug: String, owner: String, public: Boolean)
  object Doc:
    given Entity[Doc] = Entity.derived[Doc].key(_.slug)

  case class User(email: String, active: Boolean)
  object User:
    given Entity[User] = Entity.derived[User].key(_.email)

  test("define: multi-body OR-style rule with no self reference"):
    val db = Db()
    db.add(User("alice@x.io", true), User("bob@x.io", true), User("carol@x.io", false))
    db.add(
      Doc("intro", "alice@x.io", true),
      Doc("plan", "bob@x.io", false),
      Doc("memo", "alice@x.io", false)
    )

    val canRead: Rule[User, Doc] =
      Rule[User, Doc](
        (u, d) => d.owner === u.email,
        (u, d) => d.public === true && u.active === true
      )

    val alice = db.query[User, Doc] { (u, d) =>
      find(d.slug) where (u.email === "alice@x.io" && canRead(u, d))
    }.run.toSet
    assertEquals(alice, Set("intro", "memo"))

    val bob = db.query[User, Doc] { (u, d) =>
      find(d.slug) where (u.email === "bob@x.io" && canRead(u, d))
    }.run.toSet
    assertEquals(bob, Set("intro", "plan"))

    val carol = db.query[User, Doc] { (u, d) =>
      find(d.slug) where (u.email === "carol@x.io" && canRead(u, d))
    }.run.toSet
    assertEquals(carol, Set.empty[String])

  case class Place(code: String)
  object Place:
    given Entity[Place] = Entity.derived[Place].key(_.code)

  case class Road(from: Place, to: Place)
  object Road:
    given Entity[Road] = Entity.derived[Road].key(r => (r.from.code, r.to.code))

  test("recursive: reachability over an edge entity"):
    val db = Db()
    val a = Place("A"); val b = Place("B"); val c = Place("C"); val d = Place("D")
    db.add(Road(a, b), Road(b, c), Road(c, d), Road(a, d))

    val reachable: Rule[Place, Place] = Rule.recursive[Place, Place] { self =>
      Seq(
        (s, t) =>
          exists[Road](r => r.from === s && r.to === t),
        (s, t) =>
          exists[Place] { mid =>
            exists[Road](r => r.from === s && r.to === mid) && self(mid, t)
          }
      )
    }

    val fromA = db.query[Place, Place] { (s, t) =>
      find(t.code) where (s.code === "A" && reachable(s, t))
    }.run.toSet
    assertEquals(fromA, Set("B", "C", "D"))

    val fromB = db.query[Place, Place] { (s, t) =>
      find(t.code) where (s.code === "B" && reachable(s, t))
    }.run.toSet
    assertEquals(fromB, Set("C", "D"))

    val fromD = db.query[Place, Place] { (s, t) =>
      find(t.code) where (s.code === "D" && reachable(s, t))
    }.run.toSet
    assertEquals(fromD, Set.empty[String])

  test("recursive: terminates on cycles in the data"):
    val db = Db()
    val a = Place("A"); val b = Place("B"); val c = Place("C")
    db.add(Road(a, b), Road(b, c), Road(c, a)) // cycle A -> B -> C -> A

    val reachable: Rule[Place, Place] = Rule.recursive[Place, Place] { self =>
      Seq(
        (s, t) => exists[Road](r => r.from === s && r.to === t),
        (s, t) =>
          exists[Place] { mid =>
            exists[Road](r => r.from === s && r.to === mid) && self(mid, t)
          }
      )
    }

    val fromA = db.query[Place, Place] { (s, t) =>
      find(t.code) where (s.code === "A" && reachable(s, t))
    }.run.toSet
    assertEquals(fromA, Set("A", "B", "C"))
