import datomlite.*

case class Dept(
  @key code: String,
  name: String
) derives Entity

case class Emp(
  @key email: String,
  name: String,
  dept: Dept,
  salary: Long,
  skills: Set[String]
) derives Entity

val eng = Dept("ENG", "Engineering")
val sal = Dept("SAL", "Sales")

val db = Db(
  Emp("m@x.io", "Matthieu", eng, 120_000L, Set("Scala", "Haskell")),
  Emp("a@x.io", "Alice",    eng, 100_000L, Set("Scala")),
  Emp("b@x.io", "Bob",      sal, 150_000L, Set("Haskell")),
)

println(db.pretty)
println(db.prettyLog())
