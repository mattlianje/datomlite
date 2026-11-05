package datomlite

object Db {

  /** The empty database */
  val empty: Db = new DbImpl.Snap(DbImpl.State.empty)

  /** A database holding `values`. `Db()` is empty; `Db(matt, alice, dept)` seeds it in one tx
    * (mixed entity types are fine)
    */
  def apply(values: Datum*): Db = if values.isEmpty then empty else empty.add(values*)

  /** Replay whatever `storage` holds into a `Db`. Nothing is written back... for that, open a
    * `Conn(storage)`
    */
  def load(storage: Storage): Db = new DbImpl.Snap(DbImpl.State.rebuild(storage.load()))
}

/** An immutable database
  *
  * Every write returns a new `Db` and leaves this one as it was... so it's safe to share across
  * threads
  *
  * {{{
  *   val db  = Db(matt, alice)
  *   val db2 = db.upsert(matt.copy(name = "Matthieu"))
  *   db.where[Person].run   // still Matt
  * }}}
  *
  * To share one changing database between threads, or to react to commits, hold it in a `Conn`
  */
trait Db extends Writes[Db] {

  /** Row stream / single-entity entry point.
    *
    *   - `db.where[A]` is an `Iterable[A]` (`.run`, `.one`, `for x <- db.where[A] do ...`).
    *   - `db.where[A](pred)` filters via the AVET-aware predicate macro and returns a `Query[A]`.
    *
    * Single-entity lookups by eid live on `db.byEid[A](eid)` and `db.pullByEid[A](eid)(f)`.
    *
    * The typed-DSL closure form lives on a separate verb: `db.query[A] { e => find(...) where
    * (...)}`. Scala 3 overload resolution can't disambiguate `A => Boolean` from `Row[A] =>
    * QueryShape[T]` at the lambda-arity level, so they can't share `where`...
    */
  inline def where[A](inline pred: A => Boolean)(using e: Entity[A]): Query[A] =
    ${ QMacro.whereImpl[A]('this, 'pred, 'e) }

  def where[A](using e: Entity[A]): Query[A] = internal.all[A](using e)

  /** Reconstruct entity `A` from a raw eid value, or `None` if it doesn't belong to A
    */

  def byEid[A](eid: Long)(using Entity[A]): Option[A]

  /** Project the entity at `eid` through a `Pull[A]` closure...
    *
    * Returns `None` if the eid doesn't exist or doesn't belong to A
    *
    * Returns a builder so callers can write `db.pullByEid[A](eid)(f)` and let `R` infer from `f`
    */
  def pullByEid[A](eid: Long)(using e: Entity[A]): PullByEid[A] =
    new PullByEid[A](this, eid, e)

  private[datomlite] def result(rep: TxReport): Db = rep.dbAfter

  /** Short-circuit existence check.. AVET-eligible predicates probe the index.
    */
  inline def exists[A](inline pred: A => Boolean)(using e: Entity[A]): Boolean =
    ${ QMacro.existsImpl[A]('this, 'pred, 'e) }

  /** Sugar for `db.where[A].exists`
    */
  def exists[A](using e: Entity[A]): Boolean = where[A](using e).exists

  /** Count matching rows... AVET-eligible predicates probe the index.
    */
  inline def count[A](inline pred: A => Boolean)(using e: Entity[A]): Long =
    ${ QMacro.countImpl[A]('this, 'pred, 'e) }

  /** Sugar for `db.where[A].count`
    */
  def count[A](using e: Entity[A]): Long = where[A](using e).count

  /** Set diff between this Db's live state and `other`'s.
    *
    * `added` are datoms present in `other` but not here.. `retracted` are datoms here but not in
    * `other`
    *
    * Each result Datom carries the tx/time of the assert that produced it on its source side...
    * retractions flip `op` to `Op.Retract`. Computed against the live `(eid, attr, value)`
    * triples... so it works for any two dbs from the same lineage: before and after a write, two
    * branches, or `asOf` views
    */
  def diff(other: Db): DbDiff = {
    val a = this.internal.liveDatoms
    val b = other.internal.liveDatoms
    val aSet = a.iterator.map(d => (d.e, d.a, d.v)).toSet
    val bSet = b.iterator.map(d => (d.e, d.a, d.v)).toSet
    val added = b.filter(d => !aSet((d.e, d.a, d.v)))
    val retracted = a.filter(d => !bSet((d.e, d.a, d.v))).map(_.copy(op = Op.Retract))
    DbDiff(added, retracted)
  }

  /** The db as it stood right after `tx` */
  def asOf(tx: TxId): Db
  @scala.annotation.targetName("asOfTime")
  def asOf(at: Time): Db
  @scala.annotation.targetName("asOfIso")
  def asOf(at: String): Db = asOf(Time.parse(at))

  /** Full append-only log of every assert and retract ever transacted. Datoms come back in
    * insertion order; the live state is the merge of asserts minus retracts...
    */
  def log: Vector[Datom]

  /** Datoms touching the entity that currently matches `a` by structural key, in tx order...
    */
  def historyOf[A](a: A)(using Entity[A]): Vector[Datom]

  /** Datoms touching `eid`, in tx order. Survives retraction (the log keeps the trail even after
    * the entity is gone from the live view)
    */
  def history(eid: Long): Vector[Datom]

  /** Human-readable dump of the current state, grouped by entity class...
    */
  def pretty: String

  /** Tab-aligned table of the full append-only log: `tx | time | eid | attr | value | op` (`+` for
    * assert, `-` for retract)
    */
  def prettyLog(color: Boolean = false): String

  /** Macro target hatch
    */
  def internal: DbInternal

  private[datomlite] def state: DbImpl.State
}

/** Result of `db.diff(other)`..facts that the right-hand side has and the left doesn't (`added`)
  *
  * and facts the left has and the right doesn't (`retracted`). Each Datom carries the original
  * tx/time of its assert so callers can trace provenance.
  */
final case class DbDiff(
    added: Vector[Datom],
    retracted: Vector[Datom]
) {
  def isEmpty: Boolean = added.isEmpty && retracted.isEmpty
  def nonEmpty: Boolean = !isEmpty
}

/** Builder for `db.pullByEid[A](eid)(f)`...
  *
  * Splits A and R across two argument lists so callers specify only A and let R infer from the pull
  * closure
  */
final class PullByEid[A] private[datomlite] (db: Db, eid: Long, e: Entity[A]) {
  def apply[R](f: Pull[A] => R): Option[R] = {
    val ctx = db.internal.pullCtx
    if !ctx.existsAs(eid, e) then None
    else Some(f(new Pull[A](ctx, eid, e)))
  }
}
