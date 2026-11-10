import datomlite._

// Case classes are the schema
case class Fighter(@key name: String, style: String) derives Entity
case class Bout(@key id: String, winner: Fighter, loser: Fighter) derives Entity

// Adding a bout adds its fighters too
val li     = Fighter("Jet Li", "Wushu")
val gegard = Fighter("Gegard Mousasi", "Mixed")
val rory   = Fighter("Rory MacDonald", "Pankration")
val db = Db()
val t1 = db.add(Bout("B1", li, gegard), Bout("B2", gegard, rory)).tx

// Joins are typed, checked at compile time
db.query[Bout, Fighter, Fighter] { (b, w, l) =>
  find(w.name, l.name) where (b.winner === w && b.loser === l)
}.run

// Rules can be recursive
val beat = Rule.recursive[Fighter, Fighter] { self => Seq(
  (w, l) => exists[Bout](b => b.winner === w && b.loser === l),
  (w, l) => exists[Fighter](m => self(w, m) && self(m, l))
)}

// Everyone Jet Li beat, directly or not
db.query[Fighter, Fighter] { (w, l) =>
  find(l.name) where (w.name === "Jet Li" && beat(w, l))
}.run

// Speculative: what if Rory beat Jet Li? A branch, db is untouched
val whatIf = db.withTx(Tx.add(Bout("B3", rory, li)))
List(db, whatIf).map(_.where[Bout].map(_.id).toList)

// Upsert by key. Past states are still queryable
db.upsert(gegard.copy(style = "Kickboxing"))
List(db, db.asOf(t1)).map(_.where[Fighter](_.name == gegard.name).one.map(_.style))
