import datomlite._
import java.nio.file.Path

/* Case classes are the schema */
case class Fighter(
    @key name: String, // @key marks the identity field
    style: String,
    country: String
) derives Entity

case class Bout(
    @key id: String,
    red: Fighter,
    blue: Fighter,
    winner: Fighter
) derives Entity

val li     = Fighter("Jet Li",         "Wushu",      "China")
val gegard = Fighter("Gegard Mousasi", "Mixed",      "Netherlands")
val rory   = Fighter("Rory MacDonald", "Pankration", "Canada")

/* Your db is a plain value. All-or-nothing writes hand back a new one */
val db0 = Db()
val db = db0.add(
  Bout("B1", li,     gegard, li),
  Bout("B2", li,     rory,   li),
  Bout("B3", gegard, rory,   gegard),
  Bout("B4", rory,   li,     li)
)

/* Reads feel like a Scala collection */
db.where[Fighter].size // 3
db.where[Fighter](_.country == "China").map(_.name).run // Vector("Jet Li")

/* Joins are for-comprehensions */
val redWinners =
  (for
    b <- db.where[Bout]
    if b.winner == b.red
  yield b.winner.name).toSet // Set("Jet Li", "Gegard Mousasi")

/* Typed datalog for aggregations, joins, recursive rules */
val standings =
  db.query[Bout, Fighter] { (b, f) =>
    find(f.name, count(b))
      .where(b.winner === f)
      .orderBy(count(b).desc)
  }.run // Vector(("Jet Li", 3), ("Gegard Mousasi", 1))

/* Db is a value, share it freely */
given Db = db

def wins(f: Fighter)(using db: Db) =
  db.where[Bout](_.winner == f).count

wins(gegard) // 1

/* Speculative branches and time travel */
val whatIfDb = db.add(Bout("B5", li, gegard, gegard))
val dbAtTime = db.asOf("2026-01-01")

/* A threadsafe Conn holds one shared db that changes */
val conn = Conn(db)
conn.upsert(gegard.copy(style = "Kickboxing"))

/* Bring your own storage */
val persistent = Conn(FileStorage(Path.of("db.dlite")))
