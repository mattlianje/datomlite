package datomlite

import scala.language.dynamics

/** Project these terms in this order.
  *
  * `where(...)` is optional.. without it the query binds every matching row in the db. Used inside
  * a `db.query[A, B] { (e, d) => ... }` lambda where the row params are in scope
  *
  * Arity 1 returns the bare value.. arities 2..8 with positional args return a positional tuple.
  * Using named arguments lifts the result into a `NamedTuple` whose labels are the names you wrote
  * at the call site
  *
  * {{{
  *    db.query[Employee, Department] { (e, d) =>
  *      find(e.email, d.name) where (e.dept === d)
  *    }.run   // Vector[(String, String)]
  *
  *    db.query[Employee, Department] { (e, d) =>
  *      find(email = e.email, dept = d.name) where (e.dept === d)
  *    }.run   // Vector[NamedTuple[("email", "dept"), (String, String)]]
  * }}}
  *
  * `find` is a `Dynamic` object so that named arguments resolve through the `applyDynamicNamed`
  * macro instead of forcing an extra layer of parens around a tuple literal
  */
object find extends scala.Dynamic {

  /** Positional path: `find(e.name, e.salary)`.
    *
    * The macro reads each arg's `Term[t]` type and assembles a `QueryShape[T]` (bare `T` for arity
    * 1, positional tuple for arity 2..N). Going through `applyDynamic` rather than a vanilla
    * `apply` overload so that `applyDynamicNamed` (below) actually fires on named-arg calls..
    * static overloads would shadow it
    */
  transparent inline def applyDynamic(method: String)(
      inline args: Any*
  ): Any = ${ FindMacros.positional('args) }

  /** Named-args path: `find(name = e.name, salary = e.salary)`.
    *
    * Each named arg is recovered as a `(label, Term[t])` pair by the macro, which then assembles
    * the `NamedTuple` row type
    */
  transparent inline def applyDynamicNamed(method: String)(
      inline args: (String, Any)*
  ): Any = ${ FindMacros.named('args) }
}

/** Result of a typed query lambda.
  *
  * What to project, what to constrain, plus optional sort and paging. `T` is the projected row
  * type, bare value for arity 1, tuple for arity 2+, threaded through to `TypedSelector[T].run`
  *
  * `where = Constraint.True` until the user attaches one.. `orderBys` empty means insertion order..
  * `limitN` and `offsetN` are applied after sort
  */
final case class QueryShape[T] private[datomlite] (
    find: Vector[Term[?]],
    where: Constraint,
    isUnary: Boolean,
    orderBys: Vector[OrderKey] = Vector.empty,
    limitN: Option[Int] = None,
    offsetN: Int = 0
) {

  /** Attach a constraint to the query.
    *
    * Replaces any prior constraint on this shape.. compose with `&&` instead of chaining
    * `.where(...).where(...)`
    */
  infix def where(c: Constraint): QueryShape[T] = copy(where = c)

  /** Sort the result by these keys.
    *
    * Each key is `term.asc` or `term.desc`.. the term must appear in `find(...)`. Multiple keys are
    * applied left-to-right, so `orderBy(d.name.asc, e.salary.desc)` groups by department then sorts
    * each group by descending salary
    */
  def orderBy(keys: OrderKey*): QueryShape[T] = copy(orderBys = keys.toVector)

  /** Keep at most `n` rows after sort. */
  def limit(n: Int): QueryShape[T] = copy(limitN = Some(n))

  /** Skip the first `n` rows after sort. */
  def offset(n: Int): QueryShape[T] = copy(offsetN = n)
}

/** Internal... build a fresh `Row[A]` from an in-scope `Entity[A]`.
  *
  * Not user-facing.. the closure-form `db.query` allocates rows for the user
  */
private[datomlite] def freshRow[A](using e: Entity[A]): Row[A] =
  new Row[A](Row.freshBinding(e.name), e)

/** Negated subgoal.
  *
  * The constraint must reference at least one variable bound by the surrounding positive
  * conjunction so the anti-join has a join key.. bare `not(...)` over fresh row params is still
  * legal because the `find(...)` projection scopes those rows
  *
  * {{{
  *    db.query[Employee, Order] { (e, o) =>
  *      find(e.name) where (not(e.email === o.buyer))   // employees with no orders
  *    }
  * }}}
  *
  * `not(...)` may only appear in the top-level conjunction.. nesting under `||` or under another
  * `not(...)` is rejected at query build time
  */
def not(c: Constraint): Constraint = Constraint.Not(c)

/** Existentially-quantified body row for use inside a rule body or any `where(...)`.
  *
  * Allocates a fresh `Row[A]` and applies it to `body`.. the returned constraint references the row
  * through its fields and any rule calls, and the runtime treats the row as "some entity exists
  * such that"
  *
  * {{{
  *    val ancestor = Rule.recursive[Officer, Officer] { self =>
  *      Seq(
  *        (anc, desc) => desc.boss === anc.name,
  *        (anc, desc) => exists[Officer] { mid =>
  *          desc.boss === mid.name && self(anc, mid)
  *        }
  *      )
  *    }
  * }}}
  */
def exists[A](body: Row[A] => Constraint)(using e: Entity[A]): Constraint =
  body(freshRow[A])

/** Count rows in the group. */
def count[T](t: Term[T]): Term.Agg[Long] = Term.Agg[Long]("count", t)

/** Count distinct values in the group. */
def countDistinct[T](t: Term[T]): Term.Agg[Long] = Term.Agg[Long]("count-distinct", t)

/** Sum of a numeric column. Result type matches the input. */
def sum[T](t: Term[T])(using Numeric[T]): Term.Agg[T] = Term.Agg[T]("sum", t)

/** Minimum of an ordered column. */
def min[T](t: Term[T])(using Ordering[T]): Term.Agg[T] = Term.Agg[T]("min", t)

/** Maximum of an ordered column. */
def max[T](t: Term[T])(using Ordering[T]): Term.Agg[T] = Term.Agg[T]("max", t)

/** Mean of a numeric column. Always returns Double. */
def avg[T](t: Term[T])(using Numeric[T]): Term.Agg[Double] = Term.Agg[Double]("avg", t)

/** Partition-and-order spec, the right side of `<fn> over (...)`.
  *
  * Build with `partitionBy(...)` then optionally chain `.orderBy(...)`. Empty partition means one
  * group covering every row
  */
final case class WindowSpec(
    partition: Vector[Term[?]],
    order: Vector[OrderKey]
) {

  /** Within-partition sort.
    *
    * Required for `rank`, `rowNumber`, `denseRank`, `lag`, and `lead`. Aggregate windows ignore the
    * order... they compute one partition-wide total and broadcast it
    */
  def orderBy(keys: OrderKey*): WindowSpec = copy(order = keys.toVector)
}

/** Start a window spec by partition keys.
  *
  * Each key must be a `Term.Field` or `Term.RowRef` so the runtime has a stable name to group by.
  * Pass no args to compute over the entire result as one partition
  */
def partitionBy(keys: Term[?]*): WindowSpec = WindowSpec(keys.toVector, Vector.empty)

/** A window kernel before an `over(...)` is attached.
  *
  * The DSL constructors (`rank`, `rowNumber`, `denseRank`, `lag`, `lead`) all return one of these
  * so that the spec must be supplied before the value is usable in `find(...)`. `T` is the kernel's
  * result type
  */
final case class WindowFnRaw[T] private[datomlite] (fn: String, args: Vector[Term[?]])

extension [T](self: WindowFnRaw[T]) {

  /** Attach a partition-and-order spec, producing a `Term[T]` ready for `find(...)`. */
  infix def over(spec: WindowSpec): Term[T] =
    Term.Window[T](self.fn, self.args, spec.partition, spec.order)
}

extension [T](self: Term.Agg[T]) {

  /** Re-purpose an aggregation as a window.
    *
    * The aggregate is computed once per partition and broadcast to every row in the partition..
    * partition `order` is unused
    */
  infix def over(spec: WindowSpec): Term[T] =
    Term.Window[T]("agg/" + self.fn, Vector(self.inner), spec.partition, spec.order)
}

/** SQL-style row index within the partition... 1, 2, 3, ... regardless of ties.
  *
  * Requires an `orderBy(...)` on the spec
  */
def rowNumber(): WindowFnRaw[Long] = WindowFnRaw("row_number", Vector.empty)

/** SQL-style rank: ties get the same rank, the next rank skips. 1, 1, 3, 4. */
def rank(): WindowFnRaw[Long] = WindowFnRaw("rank", Vector.empty)

/** Like `rank` but no skipping after a tie. 1, 1, 2, 3. */
def denseRank(): WindowFnRaw[Long] = WindowFnRaw("dense_rank", Vector.empty)

/** Value of `t` from the row `n` positions earlier within the sorted partition.
  *
  * Returns `None` past the partition boundary. `n` defaults to 1
  */
def lag[T](t: Term[T]): WindowFnRaw[Option[T]] = WindowFnRaw("lag:1", Vector(t))
def lag[T](t: Term[T], n: Int): WindowFnRaw[Option[T]] = WindowFnRaw(s"lag:$n", Vector(t))

/** Value of `t` from the row `n` positions later within the sorted partition.
  *
  * Returns `None` past the partition boundary. `n` defaults to 1
  */
def lead[T](t: Term[T]): WindowFnRaw[Option[T]] = WindowFnRaw("lead:1", Vector(t))
def lead[T](t: Term[T], n: Int): WindowFnRaw[Option[T]] = WindowFnRaw(s"lead:$n", Vector(t))
