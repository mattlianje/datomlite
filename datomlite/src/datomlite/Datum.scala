package datomlite

/** A value paired with its `Entity` schema
  *
  * Captured at the call site of `Tx.add` / `Tx.retract`. Each argument needs an in-scope `given
  * Entity[A]`, so `Tx.add(matt, ali, team)` only compiles when every value has a known schema
  */
opaque type Datum = (Any, Entity[Any])

object Datum {
  def apply(value: Any, entity: Entity[Any]): Datum = (value, entity)

  /** `Tx.add(person)` works whenever `given Entity[Person]` is in scope. */
  given fromValue[A](using e: Entity[A]): Conversion[A, Datum] =
    a => (a, e.asInstanceOf[Entity[Any]])

  extension (d: Datum) {
    def value: Any = d._1
    def entity: Entity[Any] = d._2
  }
}
