package datomlite

import java.util.UUID

class CodecTest extends munit.FunSuite:

  enum Status derives Codec:
    case Active, Banned

  enum Shape derives Codec:
    case Circle(r: Double)
    case Rect(w: Double, h: Double)

  case class Address(street: String, city: String) derives Codec

  case class Money(cents: Long)
  given Codec[Money] = Codec.of[Money, Long](_.cents, Money(_))

  case class Account(
      id: UUID,
      status: Status,
      home: Address,
      balance: BigDecimal,
      tags: List[String],
      limits: Map[String, Int],
      shape: Shape,
      fee: Money,
      previous: Option[Address],
      badges: Set[Status]
  )
  object Account:
    given Entity[Account] = Entity.derived[Account].key(_.id)

  private val id1 = UUID.fromString("00000000-0000-0000-0000-000000000001")
  private val id2 = UUID.fromString("00000000-0000-0000-0000-000000000002")

  private val alice = Account(
    id1,
    Status.Active,
    Address("1 Main St", "Paris"),
    BigDecimal("12.50"),
    List("vip", "early"),
    Map("daily" -> 100, "monthly" -> 1000),
    Shape.Circle(1.5),
    Money(250),
    Some(Address("9 Old Rd", "Lyon")),
    Set(Status.Active)
  )
  private val bob = Account(
    id2,
    Status.Banned,
    Address("2 High St", "London"),
    BigDecimal("0.10"),
    Nil,
    Map.empty,
    Shape.Rect(2, 3),
    Money(0),
    None,
    Set.empty
  )

  private def db(): Db =
    val d = Db()
    d.add(alice, bob)
    d

  test("codec: every value type round-trips through the db"):
    assertEquals(db().where[Account].run.toSet, Set(alice, bob))

  test("codec: datoms hold only stored forms"):
    val d = db()
    val eid = d.internal.eidOf(alice).get
    val stored = d.log.filter(_.e.value == eid).map(d => d.a -> d.v).toMap
    assertEquals(stored("Account/id"), id1.toString)
    assertEquals(stored("Account/fee"), 250L)
    assertEquals(stored("Account/home"), Vector("street", "1 Main St", "city", "Paris"))
    val statuses = d.log.filter(_.a == "Account/status").map(_.v).toSet
    assertEquals(statuses, Set[Any]("Active", "Banned"))

  test("codec: plain enums store as the case name, data enums as name + payload"):
    assertEquals(Codec[Status].encode(Status.Banned), "Banned")
    assertEquals(Codec[Shape].encode(Shape.Circle(1.5)), Vector("Circle", Vector("r", 1.5)))
    assertEquals(Codec[Shape].decode(Codec[Shape].encode(Shape.Rect(1, 2))), Shape.Rect(1, 2))

  test("codec: Map and Set encode independent of insertion order"):
    val a = Codec[Map[String, Int]].encode(Map("a" -> 1, "b" -> 2))
    val b = Codec[Map[String, Int]].encode(Map("b" -> 2, "a" -> 1))
    assertEquals(a, b)

  test("codec: tuples and Either derive"):
    val c = Codec[(Int, Either[String, Long])]
    assertEquals(c.decode(c.encode((1, Right(2L)))), (1, Right(2L)))

  test("codec: where probes AVET with the encoded value"):
    val d = db()
    assertEquals(d.where[Account](_.status == Status.Banned).run, Vector(bob))
    assertEquals(d.where[Account](_.id == id1).run, Vector(alice))
    assertEquals(d.where[Account](_.home == Address("2 High St", "London")).run, Vector(bob))
    assertEquals(d.where[Account](_.badges.contains(Status.Active)).run, Vector(alice))
    assertEquals(
      d.where[Account](_.previous.contains(Address("9 Old Rd", "Lyon"))).run,
      Vector(alice)
    )

  test("codec: typed query encodes literals and decodes projections"):
    val d = db()
    val statuses = d.query[Account] { a =>
      find(a.status) where (a.fee === Money(250))
    }.run
    assertEquals(statuses, Vector(Status.Active))

    val homes = d.query[Account] { a =>
      find(a.home) where (a.status === Status.Banned)
    }.run
    assertEquals(homes, Vector(Address("2 High St", "London")))

  test("codec: comparisons and aggregates run on stored forms"):
    val d = db()
    val rich = d.query[Account] { a =>
      find(a.id) where (a.balance > BigDecimal(1))
    }.run
    assertEquals(rich, Vector(id1))
    assertEquals(d.query[Account](a => find(max(a.balance))).run, Vector(BigDecimal("12.50")))
    assertEquals(d.query[Account](a => find(sum(a.balance))).run, Vector(BigDecimal("12.60")))

  test("codec: orderBy on an enum column sorts by stored form"):
    val rows = db().query[Account] { a =>
      find(a.status, a.id).orderBy(a.status.desc)
    }.run
    assertEquals(rows.map(_._1), Vector(Status.Banned, Status.Active))

  test("codec: pull decodes scalars, options and sets"):
    val d = db()
    val eid = d.internal.eidOf(alice).get
    val out = d.pullByEid[Account](eid)(a => (a.status, a.home, a.previous, a.badges, a.fee))
    assertEquals(
      out,
      Some((Status.Active, alice.home, alice.previous, Set(Status.Active), Money(250)))
    )

  test("codec: retract by value matches the encoded row"):
    val d = db()
    d.retract(bob)
    assertEquals(d.where[Account].run, Vector(alice))
