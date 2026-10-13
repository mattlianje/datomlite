package datomlite

import java.util.concurrent.atomic.AtomicLong

/** Internal datalog IR.
  *
  * Typed queries of the form `db.query[A] { ... }` lower to these clauses, which the runtime
  * evaluator in [[DbImpl]] interprets
  */
private[datomlite] object Q {

  sealed trait Term
  final case class Var(name: String) extends Term
  final case class C(value: Any) extends Term

  object Var {
    // shared with Row's binding counter in spirit, kept separate so the $-prefixed names stay
    // visually distinct from user bindings in any printed clause
    private val counter = new AtomicLong(0)

    /** Anonymous fresh var..`val p, e, n = Var()` gives three distinct vars. */
    def apply(): Var = Var(s"$$${counter.incrementAndGet()}")
  }

  enum Op {
    case Lt, Le, Gt, Ge, Eq, Ne
  }

  /** A clause in a compiled query body.
    *
    * `Pattern` is a single triple pattern.. `Predicate` filters the current row stream.. `Closure`
    * is emitted by `Rule.reaches`
    */
  sealed trait Clause

  /** A single triple-pattern. Attribute is a literal of form `"Class/field"`
    *
    * `fallback` is bound for entities of the attribute's class with no datom for it (see
    * `Schema.fallback`). `bounds` narrows an unbound scan to a slice of the sorted AVET. It's
    * inclusive and just a hint... the comparisons it came from still run
    */
  final case class Pattern(
      e: Term,
      a: String,
      v: Term,
      fallback: Option[Any] = None,
      bounds: Bounds = Bounds.none
  ) extends Clause

  /** Inclusive range over one attribute, in stored form. `None` leaves that side open */
  final case class Bounds(lo: Option[Any], hi: Option[Any]) {
    def isEmpty: Boolean = lo.isEmpty && hi.isEmpty
    def admits(v: Any): Boolean =
      lo.forall(Compare.total(v, _) >= 0) && hi.forall(Compare.total(v, _) <= 0)

    /** Intersect two ranges, keeping the tighter side of each */
    def &(o: Bounds): Bounds = Bounds(
      (lo ++ o.lo).maxOption(using Compare.ordering),
      (hi ++ o.hi).minOption(using Compare.ordering)
    )
  }

  object Bounds {
    val none: Bounds = Bounds(None, None)
  }

  /** Predicate clause filtering the current row stream. */
  final case class Predicate(lhs: Term, op: Op, rhs: Term) extends Clause

  /** Transitive-closure clause emitted by `Rule.reaches` rule applications.
    *
    * Reads as "one or more hops along `attr` from `src` reach `dst`". Both `src` and `dst` are
    * eids; the runtime branches on which side is already bound... forward BFS from `src`, reverse
    * BFS from `dst`, pair check, or full pair enumeration when neither side is bound
    */
  final case class Closure(src: Term, attr: String, dst: Term) extends Clause

  /** Application of a user-defined rule.
    *
    * The rule reference is the `Rule` shell whose bodies have been pre-compiled to
    * `Vector[Q.Clause]`; `args` are the call-site terms (vars or consts) lined up positionally with
    * the rule's head parameters. The runtime computes a fact set per rule via fixed-point iteration
    * and binds the args from each fact, in either direction
    *
    * Held as `AnyRef` so this file stays free of an init-order tangle with `Rule`; the evaluator
    * casts it back to `Rule[?, ?]` at use site
    */
  final case class RuleCall(rule: AnyRef, args: Vector[Term]) extends Clause

  /** A find-element is either a plain var or an aggregation. */
  sealed trait FindElem {
    def name: String
  }

  final case class FindVar(name: String) extends FindElem
  final case class FindAgg(fn: String, varName: String) extends FindElem {
    def name: String = s"($fn $varName)"
  }

  /** Substitute every `Var(n)` whose name appears in `binding` with the bound value as `C(value)`.
    *
    * Used to seed an anti-join subgoal with the outer disjunct's binding so the negated subquery
    * sees the same row-scope variables as constants
    */
  def substituteClauses(clauses: Vector[Clause], binding: Map[String, Any]): Vector[Clause] = {
    def sub(t: Term): Term = t match {
      case v: Var => binding.get(v.name).map(C(_)).getOrElse(v)
      case c: C   => c
    }
    clauses.map {
      case p: Pattern   => p.copy(e = sub(p.e), v = sub(p.v))
      case p: Predicate => Predicate(sub(p.lhs), p.op, sub(p.rhs))
      case c: Closure   => Closure(sub(c.src), c.attr, sub(c.dst))
      case r: RuleCall  => RuleCall(r.rule, r.args.map(sub))
    }
  }

  def aggregate(fn: String, values: Vector[Any]): Any = fn match {
    case "count"          => values.size.toLong
    case "count-distinct" => values.toSet.size.toLong
    case "sum"            => sumOf(values)
    case "min" => values.min(using Ordering.fromLessThan((a, b) => Compare.any(a, b) < 0))
    case "max" => values.max(using Ordering.fromLessThan((a, b) => Compare.any(a, b) < 0))
    case "avg" =>
      if values.isEmpty then 0.0 else values.map(toDouble).sum / values.size
    case other => sys.error(s"unknown aggregation: $other")
  }

  private def sumOf(values: Vector[Any]): Any =
    if values.isEmpty then 0L
    else
      values.head match {
        case _: Long       => values.foldLeft(0L)((a, v) => a + v.asInstanceOf[Long])
        case _: Int        => values.foldLeft(0L)((a, v) => a + v.asInstanceOf[Int].toLong)
        case _: Double     => values.foldLeft(0.0)((a, v) => a + v.asInstanceOf[Double])
        case _: BigDecimal => values.foldLeft(BigDecimal(0))((a, v) => a + toBigDecimal(v))
        case _: BigInt     => values.foldLeft(BigInt(0))((a, v) => a + v.asInstanceOf[BigInt])
        case _             => values.map(toDouble).sum
      }

  private def toDouble(v: Any): Double = v match {
    case l: Long       => l.toDouble
    case i: Int        => i.toDouble
    case d: Double     => d
    case f: Float      => f.toDouble
    case b: BigDecimal => b.toDouble
    case b: BigInt     => b.toDouble
    case other         => sys.error(s"cannot aggregate non-numeric: $other")
  }

  private def toBigDecimal(v: Any): BigDecimal = v match {
    case b: BigDecimal => b
    case b: BigInt     => BigDecimal(b)
    case other         => BigDecimal(toDouble(other))
  }
}
