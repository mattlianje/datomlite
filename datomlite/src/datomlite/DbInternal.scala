package datomlite

/** Macro-target and internal escape hatches.
  *
  * Reachable as `db.internal` so the public `Db` trait stays small. Everything here is callable
  * from generated code (the splice site lives outside `datomlite`, so the methods must be public),
  * but is not part of the user-facing surface
  *
  * Stability is not guaranteed... signatures change as the macros evolve
  */
trait DbInternal {

  /** Hash-probe via the AVET index by raw string attribute name.
    *
    * Emitted by `where` when it detects an equality predicate
    */
  def byEqByName[A](attr: String, value: Any)(using Entity[A]): Query[A]

  /** Set of raw eid values whose `attr` (in `"Class/field"` form) equals `value`.
    *
    * Lower level than `byEqByName`.. emitted by macros when chaining ref hops
    */
  def eidsByEq(attr: String, value: Any): Set[Long]

  /** Class-scoped eid probe.
    *
    * Finds eids of `A` whose `attr` field (bare name) equals `value`. Composes `Class/attr`
    * internally. Emitted by the pull macros for indexed predicates
    */
  def eidsByEqByName[A](attr: String, value: Any)(using Entity[A]): Set[Long]

  /** All eids currently belonging to class `A`. Used by the pull macros' scan fallback. */
  def allEids[A](using Entity[A]): Set[Long]

  /** Find the eid currently storing `a` by structural match.
    *
    * Used by `Query.pull` to resolve each row to its eid before binding a `Pull[A]`
    */
  def eidOf[A](a: A)(using Entity[A]): Option[Long]

  /** Snap a fresh `PullCtx` against the current state.
    *
    * Each pull entry point grabs one of these so lazy field reads inside a closure all see the same
    * world
    */
  def pullCtx: PullCtx

  /** Run `f` over each eid as a `Pull[A]`. Internal helper for the predicate-pull macros. */
  def pullManyEids[A, R](eids: Iterable[Long])(f: Pull[A] => R)(using Entity[A]): Vector[R]

  /** Evaluate a vector of clauses against the current state, returning the raw row stream.
    *
    * Lowered to from the typed query DSL (`db.query[A] { ... }`)
    */
  def evalClauses(clauses: Vector[Q.Clause]): Iterator[Map[String, Any]]

  /** Full-class scan as a `Query[A]`. Backs the no-arg `db.where[A]` row stream. */
  def all[A](using Entity[A]): Query[A]

  /** Currently-live datoms.
    *
    * Each `(eid, attr, value)` triple still in state, paired with the most recent `Op.Assert` that
    * put it there. Used by `Db.diff`
    */
  def liveDatoms: Vector[Datom]
}
