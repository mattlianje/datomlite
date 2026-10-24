package datomlite

opaque type TxId = Long
object TxId {
  def apply(n: Long): TxId = n
  extension (t: TxId) def value: Long = t
}

enum Op {
  case Assert, Retract
}

sealed trait Tx
object Tx {
  final case class Add(values: Vector[Datum]) extends Tx
  final case class Retract(values: Vector[Datum]) extends Tx

  /** Upsert step
    *
    * Asserts each value, but on a `.key` collision retracts the prior datoms and re-asserts under
    * the same eid. Refs that pointed at the old eid stay pointing at the new value
    *
    * `unique`-but-not-key violations across other entities still throw `UniqueViolation`
    */
  final case class Upsert(values: Vector[Datum]) extends Tx

  /** Multi-step tx
    *
    * Each step runs in declared order against the state-so-far, all sharing one tx id and time Any
    * failure aborts the whole batch.. the live state is untouched
    */
  final case class Batch(steps: Vector[Tx]) extends Tx

  /** A tx computed inside the commit
    *
    * `f` gets the state-so-far (earlier steps of the batch included) and returns the `Tx` to apply.
    * Writers go one at a time so nothing lands between the read and the write... read-modify-write
    * without lost updates
    *
    * `f` runs once under the write lock so keep it quick and pure. Read through its argument and
    * never `transact` from inside it (that throws)
    */
  final case class Fn(f: Db => Tx) extends Tx

  def add(values: Datum*): Tx = Add(values.toVector)
  def retract(values: Datum*): Tx = Retract(values.toVector)
  def upsert(values: Datum*): Tx = Upsert(values.toVector)
  def batch(steps: Tx*): Tx = Batch(steps.toVector)
  def fn(f: Db => Tx): Tx = Fn(f)
}

/** Mutable accumulator for `db.tx { b => b.add(...); b.retract(...) }`
  *
  * Each call appends a step.. the surrounding `tx` block transacts them as one `Tx.Batch`
  *
  * `db` is the state the block started from. The block runs inside the commit so no other writer
  * can sneak in. `retractWhere` / `upsertWhere` match against the state-so-far at their own step
  */
final class TxBuilder private[datomlite] (val db: Db) {
  private val buf = scala.collection.mutable.ArrayBuffer.empty[Tx]
  def add(values: Datum*): Unit = { buf += Tx.add(values*); () }
  def retract(values: Datum*): Unit = { buf += Tx.retract(values*); () }
  def upsert(values: Datum*): Unit = { buf += Tx.upsert(values*); () }

  /** Append any pre-built `Tx` as a step
    *
    * Used by the inline `retractWhere` / `upsertWhere` and available as an escape hatch when
    * callers already hold a `Tx` value
    */
  def step(tx: Tx): Unit = { buf += tx; () }

  /** Predicate-based retract step
    *
    * Retracts every row matching `pred` in the state-so-far, so rows added by earlier steps count.
    * AVET-eligible predicates probe the index
    */
  inline def retractWhere[A](inline pred: A => Boolean)(using e: Entity[A]): Unit =
    step(Tx.Fn { s =>
      Tx.Retract(s.where[A](pred).run.map(a => Datum(a, e.asInstanceOf[Entity[Any]])))
    })

  /** Predicate-based upsert step
    *
    * Runs `f` on every row matching `pred` in the state-so-far and upserts the results.
    * AVET-eligible predicates probe the index
    *
    * The upsert resolves by `.key`, so changing the key field in `f` will create a new row instead
    * of updating the matched one
    */
  inline def upsertWhere[A](inline pred: A => Boolean)(f: A => A)(using e: Entity[A]): Unit =
    step(Tx.Fn { s =>
      Tx.Upsert(s.where[A](pred).run.map(a => Datum(f(a), e.asInstanceOf[Entity[Any]])))
    })

  private[datomlite] def steps: Vector[Tx] = buf.toVector
}

/** What a commit did
  *
  * `datoms` are the log entries it appended, in order. `adds` / `retracts` are the values as handed
  * in, with tx fns already run
  */
final case class TxReport(
    tx: TxId,
    time: Time,
    adds: Vector[Datum],
    retracts: Vector[Datum],
    addedEids: Vector[Long],
    datoms: Vector[Datom] = Vector.empty
)

/** Raised by `transact` when an assert violates a `.key` or `.unique` constraint
  *
  * The whole transaction is aborted.. state is left untouched
  */
final class UniqueViolation(
    val entityName: String,
    val constraint: String,
    val value: Any
) extends RuntimeException(s"$constraint violation on $entityName: '$value' is already taken")
