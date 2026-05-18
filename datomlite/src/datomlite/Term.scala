package datomlite

/** Typed handle to a value at some position in a query. `T` is the value's static type. */
trait Term[T] {
  private[datomlite] def lower: Q.Term
}

object Term {

  /** A literal lifted into the query. The value is checked against `T` at the call site. */
  final case class Const[T](value: T) extends Term[T] {
    private[datomlite] def lower: Q.Term = Q.C(value)
  }

  /** A field projection from a row binding: `e.dept` becomes `Field(eBinding, "Worker/dept")`. */
  final case class Field[T](rowBinding: String, attr: String) extends Term[T] {
    private[datomlite] def lower: Q.Term = Q.Var(s"$rowBinding::$attr")
  }

  /** A row reference: stands for the entity's eid in patterns. `Row[A]` extends this. */
  trait RowRef[T] extends Term[T] {
    private[datomlite] def binding: String
    private[datomlite] def lower: Q.Term = Q.Var(binding)
  }

  /** Aggregation over an inner term
    *
    * Only valid in a `find(...)` projection. The inner term must be a `Field` or `RowRef` so the
    * evaluator has a binding to group/reduce
    */
  final case class Agg[T](fn: String, inner: Term[?]) extends Term[T] {
    private[datomlite] def lower: Q.Term =
      throw new RuntimeException("Term.Agg has no Q.Term lowering; only valid in find(...)")
  }

  /** Window function over a partition of the binding stream
    *
    * The runtime keeps row count intact (no collapsing) and computes one value per binding
    */
  final case class Window[T](
      fn: String,
      args: Vector[Term[?]],
      partition: Vector[Term[?]],
      order: Vector[OrderKey]
  ) extends Term[T] {
    private[datomlite] def lower: Q.Term =
      throw new RuntimeException("Term.Window has no Q.Term lowering; only valid in find(...)")
  }
}

extension [T](self: Term[T]) {
  infix def ===(other: Term[T]): Constraint = Constraint.Eq(self, other)
  infix def !==(other: Term[T]): Constraint = Constraint.Cmp(Q.Op.Ne, self, other)
  def >(other: Term[T]): Constraint = Constraint.Cmp(Q.Op.Gt, self, other)
  def >=(other: Term[T]): Constraint = Constraint.Cmp(Q.Op.Ge, self, other)
  def <(other: Term[T]): Constraint = Constraint.Cmp(Q.Op.Lt, self, other)
  def <=(other: Term[T]): Constraint = Constraint.Cmp(Q.Op.Le, self, other)

  infix def ===(value: T): Constraint = Constraint.Eq(self, Term.Const(value))
  infix def !==(value: T): Constraint = Constraint.Cmp(Q.Op.Ne, self, Term.Const(value))
  def >(value: T): Constraint = Constraint.Cmp(Q.Op.Gt, self, Term.Const(value))
  def >=(value: T): Constraint = Constraint.Cmp(Q.Op.Ge, self, Term.Const(value))
  def <(value: T): Constraint = Constraint.Cmp(Q.Op.Lt, self, Term.Const(value))
  def <=(value: T): Constraint = Constraint.Cmp(Q.Op.Le, self, Term.Const(value))

  /** Ascending sort key over this term. Use inside `orderBy(...)`. */
  def asc: OrderKey = OrderKey(self, desc = false)

  /** Descending sort key over this term. Use inside `orderBy(...)`. */
  def desc: OrderKey = OrderKey(self, desc = true)
}

/** Sort direction over a `Term[?]`.
  *
  * Built from `term.asc` / `term.desc`.. consumed by `QueryShape.orderBy(...)` and inside
  * `WindowSpec.orderBy(...)`. The term must appear in the surrounding `find(...)` projection
  */
final case class OrderKey(term: Term[?], desc: Boolean)

/** Card-many membership.
  *
  * `en.skills === sk` binds `sk` to each member of `en.skills`. The lowering is the same single
  * triple-pattern as ref-equality, since card-many fans out one datom per element
  */
extension [T](self: Term[Set[T]]) {
  @scala.annotation.targetName("eqMember")
  infix def ===(other: Term[T]): Constraint = Constraint.Eq(self, other)
}

/** Logical constraint over Terms
  *
  * Compiled to `Q.Clause`s at query time. `||` distributes over `&&` via DNF expansion at compile
  * time.. each disjunct runs as an independent pass and the results are concatenated
  */
sealed trait Constraint {
  def &&(other: Constraint): Constraint = Constraint.And(this, other)
  def ||(other: Constraint): Constraint = Constraint.Or(this, other)
}

object Constraint {
  final case class Eq(lhs: Term[?], rhs: Term[?]) extends Constraint
  final case class Cmp(op: Q.Op, lhs: Term[?], rhs: Term[?]) extends Constraint
  final case class And(a: Constraint, b: Constraint) extends Constraint
  final case class Or(a: Constraint, b: Constraint) extends Constraint

  /** Negated subgoal
    *
    * Datalog-safe... `not(...)` may only sit in the top-level conjunction, never inside `||`, and
    * may not nest. `Compile` validates both rules and errors with a clear message at query build
    * time
    */
  final case class Not(inner: Constraint) extends Constraint

  /** Application of a `Rule`
    *
    * Lowers to either a `Q.Closure` clause (`Rule.reaches`) or a `Q.RuleCall` clause (`Rule(...)` /
    * `Rule.recursive`). Built only by `Rule.apply`.. users never construct this directly
    */
  final case class RuleApp(rule: Rule[?, ?], src: Term.RowRef[?], dst: Term.RowRef[?])
      extends Constraint

  /** No-op constraint. Used as the default when a query is built without `.where(...)`. */
  case object True extends Constraint

  /** Disjunctive normal form... `Vector[Vector[Constraint]]`
    *
    * The outer vector is the OR, each inner vector is the AND of leaves (Eq, Cmp, Not). `True`
    * collapses to a single empty conjunction, which matches everything
    *
    * `Not` stays as a leaf here.. `Compile` peels it off and lowers its inner to a separate
    * anti-join subgoal
    */
  private[datomlite] def dnf(c: Constraint): Vector[Vector[Constraint]] = c match {
    case True      => Vector(Vector.empty)
    case And(a, b) => for da <- dnf(a); db <- dnf(b) yield da ++ db
    case Or(a, b)  => dnf(a) ++ dnf(b)
    case other     => Vector(Vector(other))
  }
}
