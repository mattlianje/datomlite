package datomlite

/** The write verbs `Db` and `Conn` share
  *
  * On a `Db` each one returns the new `Db` and leaves the old one as it was. On a `Conn` it commits
  * and returns the `TxReport`. Either way a call is one transaction: all of it lands or none does
  */
trait Writes[R] {

  /** Apply `tx` and report what it did. `rep.dbAfter` is the resulting `Db` */
  def transact(tx: Tx): TxReport

  /** What a write verb hands back for the report `transact` made */
  private[datomlite] def result(rep: TxReport): R

  /** Assert values. Mixed entity types are fine: each arg converts to a `Datum` via its in-scope
    * `Entity`
    */
  def add(values: Datum*): R = result(transact(Tx.add(values*)))

  /** Retract values by full equality. Misses are silent. If you only have a key, prefer
    * `retractWhere[A](_.field == key)`
    */
  def retract(values: Datum*): R = result(transact(Tx.retract(values*)))

  /** Assert values, replacing whatever holds the same `.key`
    *
    * On a collision the prior datoms are retracted and the new value re-asserted under the same
    * eid, so refs survive the swap. Without `.key` on the entity, behaves like `add`
    */
  def upsert(values: Datum*): R = result(transact(Tx.upsert(values*)))

  /** Retract every match of `pred` in one tx. Equality and range predicates take the AVET index */
  inline def retractWhere[A](inline pred: A => Boolean)(using e: Entity[A]): R =
    result(transact(Tx.retractWhere[A](pred)))

  /** Run `f` on every match of `pred` and upsert the results in one tx
    *
    * The read happens inside the commit, so on a `Conn` concurrent `upsertWhere(...)(bump)` calls
    * never lose an update. The upsert goes by `.key` so `f` should leave the key alone... changing
    * it inserts a new row
    */
  inline def upsertWhere[A](inline pred: A => Boolean)(f: A => A)(using e: Entity[A]): R =
    result(transact(Tx.upsertWhere[A](pred)(f)))

  /** Mix `add`, `retract`, `upsert`, `retractWhere` and `upsertWhere` calls on `b`. They all land
    * under one tx id and either all commit or none do. Steps run in declared order against the
    * state-so-far, so retract-then-add of the same key is safe. Read through `b.db`
    */
  def tx(build: TxBuilder => Unit): R = result(transact(Tx.block(build)))
}
