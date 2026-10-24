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

  def add(values: Datum*): Tx = Add(values.toVector)
  def retract(values: Datum*): Tx = Retract(values.toVector)
  def upsert(values: Datum*): Tx = Upsert(values.toVector)
  def batch(steps: Tx*): Tx = Batch(steps.toVector)
}

/** Mutable accumulator for `db.tx { b => b.add(...); b.retract(...) }`
  *
  * Each call appends a step.. the surrounding `tx` block transacts them as one `Tx.Batch`
  *
  * `db` is the live Db the block is running against. `retractWhere` / `upsertWhere` evaluate their
  * predicates against this Db's current state (not the state-so-far inside the block), then append
  * a resolved `Tx.Retract` / `Tx.Upsert` step
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
    * Finds every row matching `pred` against the live db and appends a retract step for them
    * AVET-eligible predicates probe the index
    */
  inline def retractWhere[A](inline pred: A => Boolean)(using e: Entity[A]): Unit = {
    val matches = db.where[A](pred).run
    val datums = matches.map(a => Datum(a, e.asInstanceOf[Entity[Any]]))
    step(Tx.Retract(datums))
  }

  /** Predicate-based upsert step
    *
    * Finds every row matching `pred`, transforms each through `f`, and appends an upsert step The
    * predicate runs against the live db (AVET-eligible predicates probe the index)
    *
    * The upsert resolves by `.key`, so changing the key field in `f` will create a new row instead
    * of updating the matched one
    */
  inline def upsertWhere[A](inline pred: A => Boolean)(f: A => A)(using e: Entity[A]): Unit = {
    val matches = db.where[A](pred).run.map(f)
    val datums = matches.map(a => Datum(a, e.asInstanceOf[Entity[Any]]))
    step(Tx.Upsert(datums))
  }

  private[datomlite] def steps: Vector[Tx] = buf.toVector
}

final case class TxReport(
    tx: TxId,
    time: Time,
    adds: Vector[Datum],
    retracts: Vector[Datum],
    addedEids: Vector[Long]
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
