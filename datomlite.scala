import datomlite._

/* An empty db */
val db = Db()

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

/* Threadsafe, all-or-nothing transactions */
val rep = db.add(
  li,
  gegard,
  rory,
  Bout("B1", li,     gegard, li),
  Bout("B2", li,     rory,   li),
  Bout("B3", gegard, rory,   gegard),
  Bout("B4", rory,   li,     li)
)

/* Reads feel like a Scala collection */
db.where[Fighter].size // 3
db.where[Fighter](_.country == "China").map(_.name) // Vector("Jet Li")

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
val whatIfDb = db.withTx(Tx.add(Bout("B5", li, gegard, li)))
val dbAtTime = db.asOf("2026-01-01")

/* Bring your own storage */
val persistent = Db(FileStorage(Path.of("db.dlite")))
