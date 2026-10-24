package datomlite

object Db {

  /** Fresh in-memory DB. `Db()` is empty; `Db(matt, alice, dept)` seeds the initial state in one
    * call (mixed entity types are fine).
    */
  def apply(values: Datum*): Db = apply(Storage.none, values*)

  /** Persistent DB backed by `storage`.
    *
    * On boot, replays whatever the storage holds to recover prior state. Each subsequent `transact`
    * appends its new datoms via `storage.append` before returning.
    *
    * Seed `values` are asserted after load; structurally identical seeds dedupe under `.key` and
    * are no-ops, so re-running `Db(file, matt)` is safe.
    */
  def apply(storage: Storage, values: Datum*): Db = {
    val db = new DbImpl.Live(storage)
    if values.nonEmpty then db.add(values*)
    db
  }
}

trait Db {

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
    * The pull context is snapped at call time, so reads inside the closure see one consistent
    * world. Returns a builder so callers can write `db.pullByEid[A](eid)(f)` and let `R` infer from
    * `f`
    */
  def pullByEid[A](eid: Long)(using e: Entity[A]): PullByEid[A] =
    new PullByEid[A](this, eid, e)

  def transact(tx: Tx): TxReport

  /** Sugar for `transact(Tx.add(values*))`...
    *
    * Mixed entity types are fine: every arg is converted to a `Datum` independently via its
    * in-scope `Entity`
    */
  def add(values: Datum*): TxReport = transact(Tx.add(values*))

  /** Sugar for `transact(Tx.retract(values*))`...
    *
    * Each value is matched by full equality; if you only have a key, prefer
    * `retractWhere[A](_.field == key)`
    */
  def retract(values: Datum*): TxReport = transact(Tx.retract(values*))

  /** Sugar for `transact(Tx.upsert(values*))`...
    *
    * On a `.key` collision the prior datoms are retracted and the new value re-asserted under the
    * same eid, so refs survive the swap. Without `.key` on the entity, behaves like `add`
    */
  def upsert(values: Datum*): TxReport = transact(Tx.upsert(values*))

  /** Predicate-based retract
    *
    * Finds all matches and retracts them in one tx... the read and the write are atomic. Equality
    * and range predicates take the AVET index
    */
  inline def retractWhere[A](inline pred: A => Boolean)(using e: Entity[A]): TxReport =
    ${ QMacro.retractWhereImpl[A]('this, 'pred, 'e) }

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

  /** Multi-op atomic block...
    *
    * Mix `add`, `retract`, and `upsert` calls inside one closure; they all land under a single tx
    * id and either all commit or none do (a `UniqueViolation` aborts the whole block)
    *
    * Steps run in declared order against the state-so-far, so retract-then-add of the same key is
    * safe. The block runs inside the commit so reads through `b.db` can't go stale. Don't transact
    * on the outer db from in here
    */
  def tx(build: TxBuilder => Unit): TxReport =
    transact(Tx.Fn { s =>
      val b = new TxBuilder(s)
      build(b)
      Tx.Batch(b.steps)
    })

  /** Predicate-based upsert...
    *
    * Runs `f` on every match and upserts the results in one tx. The read happens inside the commit
    * so concurrent `upsertWhere(...)(bump)` calls never lose an update. Equality and range
    * predicates take the AVET index. The upsert goes by `.key` so `f` should leave the key alone...
    * changing it inserts a new row
    */
  inline def upsertWhere[A](inline pred: A => Boolean)(f: A => A)(using e: Entity[A]): TxReport =
    ${ QMacro.upsertWhereImpl[A]('this, 'pred, 'f, 'e) }

  /** Set diff between this Db's live state and `other`'s.
    *
    * `added` are datoms present in `other` but not here.. `retracted` are datoms here but not in
    * `other`
    *
    * Each result Datom carries the tx/time of the assert that produced it on its source side...
    * retractions flip `op` to `Op.Retract`. Computed against the live `(eid, attr, value)`
    * triples... so it works for snapshots, `withTx` branches, and `asOf` views from the same
    * lineage
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

  /** Speculative apply: returns a new Db with `tx` applied; the live Db is untouched. */
  def withTx(tx: Tx): Db
  def snapshot: Db
  def asOf(tx: TxId): Db
  @scala.annotation.targetName("asOfTime")
  def asOf(at: Time): Db
  @scala.annotation.targetName("asOfIso")
  def asOf(at: String): Db = asOf(Time.parse(at))

  /** Call `f` after every commit, in tx order
    *
    * Runs on the committing thread once the commit is visible and never under the write lock... so
    * `f` can read or even transact. If `f` throws the error goes to the default reporter and the tx
    * and other listeners carry on
    */
  def listen(f: TxReport => Unit): Subscription

  /** A live query. Calls `f` with `q`'s result now... then again whenever it changes
    *
    * {{{
    *   db.watch(_.where[Order](_.status == Status.Open).count)(n => badge.set(n))
    * }}}
    *
    * `q` only re-runs after commits that write an entity class it read. So read through its
    * argument. Closing over the outer `db` hides what it reads and the watch re-runs on every
    * commit. End the query with `.run`, `.count` etc since a lazy `Query` never compares equal
    *
    * Callbacks run like `listen`'s
    */
  def watch[T](q: Db => T)(f: T => Unit)(using Eager[T]): Subscription

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
