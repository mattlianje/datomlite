package datomlite

/** A lazily-evaluated stream of `A` values.
  *
  * Carries the originating `Db` so projection methods (`pull`, `pullOne`) can snap a `PullCtx` and
  * resolve eids. `where` / `map` / `flatMap` / `withFilter` keep the same db, so further `pull`
  * calls still work after composition
  *
  * Extends `Iterable[A]`... `.toList`, `.size`, `.head`, `for x <- q do ...`, and the rest of the
  * standard collection vocabulary work directly. `.run` is a `Vector` shorthand for the common
  * terminal call; `.one` is a shorter alias for `headOption`
  */
class Query[A] private[datomlite] (
    private[datomlite] val source: () => Iterator[A],
    val db: Db
) extends Iterable[A] {

  def iterator: Iterator[A] = source()

  def where(p: A => Boolean): Query[A] = new Query(() => source().filter(p), db)

  override def filter(p: A => Boolean): Query[A] = where(p)
  override def filterNot(p: A => Boolean): Query[A] = where(a => !p(a))
  override def map[B](f: A => B): Query[B] = new Query(() => source().map(f), db)
  override def flatMap[B](f: A => IterableOnce[B]): Query[B] =
    new Query(() => source().flatMap(f), db)

  /** Group rows by a derived key.
    *
    * Narrows the return type of `Iterable.groupBy` to `Grouped[K, A]` so the `.agg(f)` extension is
    * available on the result
    */
  override def groupBy[K](f: A => K): Grouped[K, A] = source().toVector.groupBy(f)

  /** Force the stream into a `Vector`. Same as `toVector`, kept as a short stable name. */
  def run: Vector[A] = source().toVector

  /** At most one row, taken from the head of the stream. Same as `headOption`. */
  def one: Option[A] = source().nextOption()

  /** Row count as `Long`. Distinct in arity from `Iterable.count(p: A => Boolean): Int`. */
  def count: Long = source().size.toLong

  /** True if the stream has at least one row. Same as `nonEmpty`. */
  def exists: Boolean = source().hasNext

  /** Project each row through a `Pull[A]` closure.
    *
    * Snaps the db once, so every call inside the closure sees the same world. Each row's eid is
    * resolved by structural match
    */
  def pull[R](f: Pull[A] => R)(using e: Entity[A]): Vector[R] =
    pulled(f).toVector

  /** Pull the first matching row, or `None`. */
  def pullOne[R](f: Pull[A] => R)(using e: Entity[A]): Option[R] =
    pulled(f).nextOption()

  private def pulled[R](f: Pull[A] => R)(using e: Entity[A]): Iterator[R] = {
    val snap = db.snapshot
    val ctx = snap.internal.pullCtx
    source().flatMap(a => snap.internal.eidOf[A](a).map(eid => f(new Pull[A](ctx, eid, e))))
  }

  /** Count rows by a derived key. Shorthand for `groupBy(f).view.mapValues(_.size.toLong).toMap`.
    */
  def countBy[K](f: A => K): Map[K, Long] =
    source().foldLeft(Map.empty[K, Long]) { (m, a) =>
      val k = f(a)
      m.updated(k, m.getOrElse(k, 0L) + 1L)
    }

  /** Sort ascending by a derived key. Materializes once, then re-streams the sorted vector. */
  def orderBy[B](f: A => B)(using ord: Ordering[B]): Query[A] =
    new Query(() => source().toVector.sortBy(f).iterator, db)

  /** Sort descending by a derived key. */
  def orderByDesc[B](f: A => B)(using ord: Ordering[B]): Query[A] =
    new Query(() => source().toVector.sortBy(f)(using ord.reverse).iterator, db)

  /** Keep at most `n` rows. */
  def limit(n: Int): Query[A] = new Query(() => source().take(n), db)

  /** Skip the first `n` rows. */
  def offset(n: Int): Query[A] = new Query(() => source().drop(n), db)
}

object Query {

  /** Build a `Query[A]` from a thunk producing an iterator.
    *
    * The macro-routed AVET probes wrap pre-computed results through this entry point
    */
  def apply[A](db: Db, src: () => Iterator[A]): Query[A] = new Query(src, db)
}

/** Result of `Query[A].groupBy(f)`.
  *
  * A plain `Map[K, Vector[A]]`, plus the `.agg(f)` extension for folding each group into a single
  * value
  */
type Grouped[K, A] = Map[K, Vector[A]]

extension [K, A](g: Grouped[K, A]) {

  /** Apply a fold to each group.
    *
    * `db.where[Order].groupBy(_.buyer).agg(_.map(_.price).sum)` returns a `Map[K, Long]`
    */
  def agg[B](f: Vector[A] => B): Map[K, B] = g.view.mapValues(f).toMap
}
