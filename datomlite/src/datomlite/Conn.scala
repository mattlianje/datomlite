package datomlite

object Conn {

  /** An in-memory connection on the empty db */
  def apply(): Conn = apply(Db.empty)

  /** An in-memory connection starting from `db` */
  def apply(db: Db): Conn = new Conn(db, Storage.none)

  /** A connection backed by `storage`
    *
    * Replays whatever the storage holds to recover prior state. Each commit appends its new datoms
    * via `storage.append` before anyone can see it
    */
  def apply(storage: Storage): Conn = new Conn(Db.load(storage), storage)
}

/** A database that changes: the current `Db`, swapped for a new one on every commit
  *
  * {{{
  *   val conn = Conn()
  *   conn.add(matt, alice)
  *   conn.db.where[Person].run
  *   conn.watch(_.count[Person])(n => println(s"$n people"))
  * }}}
  *
  * `db` is a plain value, so a read never locks and never sees a half-done commit. Writers take a
  * lock one at a time so a commit (tx fns included) sees every commit before it. New datoms hit
  * `storage` before the new db is published... so a failed append loses nothing anyone saw
  */
final class Conn private (init: Db, storage: Storage) extends Writes[TxReport] with AutoCloseable {
  @volatile private var current: Db = init
  private val hub = new Hub
  private val writeLock = new Object

  /** Set during a commit so a tx fn or block that transacts on this conn fails fast */
  private var committing = false

  /** The db right now. A value, so later commits don't touch it */
  def db: Db = current

  def transact(tx: Tx): TxReport = {
    val report = writeLock.synchronized {
      if committing then
        throw new IllegalStateException(
          "transact called from inside a transaction on the same conn; return a Tx from the " +
            "tx fn (or add a step to the tx block) instead"
        )
      committing = true
      try {
        val rep = current.transact(tx)
        if rep.datoms.nonEmpty then storage.append(rep.datoms)
        current = rep.dbAfter
        hub.committed(rep, rep.dbAfter.state)
        rep
      } finally committing = false
    }
    hub.drain()
    report
  }

  private[datomlite] def result(rep: TxReport): TxReport = rep

  /** Call `f` after every commit, in tx order
    *
    * Runs on the committing thread once the commit is visible and never under the write lock... so
    * `f` can read or even transact. If `f` throws the error goes to the default reporter and the tx
    * and other listeners carry on
    */
  def listen(f: TxReport => Unit): Subscription = hub.listen(f)

  /** A live query. Calls `f` with `q`'s result now... then again whenever it changes
    *
    * {{{
    *   conn.watch(_.where[Order](_.status == Status.Open).count)(n => badge.set(n))
    * }}}
    *
    * `q` only re-runs after commits that write an entity class it read. So read through its
    * argument. Closing over some other db hides what it reads and the watch re-runs on every
    * commit. End the query with `.run`, `.count` etc since a lazy `Query` never compares equal
    *
    * Callbacks run like `listen`'s
    */
  def watch[T](q: Db => T)(f: T => Unit)(using Eager[T]): Subscription = {
    /* Under the lock so no commit lands between the first run's state and registering */
    val sub = writeLock.synchronized(hub.watch(q, f, current.state))
    hub.drain()
    sub
  }

  def close(): Unit = storage.close()

  override def toString: String = s"Conn($current)"
}
