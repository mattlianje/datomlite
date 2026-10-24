package datomlite

/** Compile a `QueryShape` into `(disjuncts, projection)` for the Q evaluator
  */
private[datomlite] object Compile {

  /** A single compiled disjunct.
    *
    * Positive clauses to evaluate, plus zero or more negative subgoals (each a clause-vector) that
    * must produce no rows after substituting the positive binding for the disjunct to keep that
    * binding.
    *
    * Multiple negatives behave like De Morgan... each represents one DNF disjunct of the inner
    * negated constraint, all of which must be empty
    */
  final case class Disjunct(
      positive: Vector[Q.Clause],
      negatives: Vector[Vector[Q.Clause]]
  )

  /** Compiled view of a `QueryShape` ready for `TypedSelector` to execute
    *
    * `orderByIdx` is a vector of `(column index in find, descending?)` pairs... `Compile` resolves
    * each `OrderKey` to its projected column at build time and errors out if the term isn't in
    * `find(...)`.
    *
    * The find list is forwarded as-is so `TypedSelector` can branch projection on Field / RowRef /
    * Agg / Window without a separate IR
    */
  final case class Output(
      disjuncts: Vector[Disjunct],
      finds: Vector[Term[?]],
      orderByIdx: Vector[(Int, Boolean)],
      limitN: Option[Int],
      offsetN: Int
  )

  /** Lower a rule body into `Q.Clause`s.
    *
    * Used by `Rule(...)` / `Rule.recursive`. The head rows carry the rule's parameter bindings...
    * their field references inside `conjuncts` produce the patterns that bind their eids, so we
    * don't need an explicit ensure-bound pass on them.
    *
    * Rejects `not(...)` inside rule bodies... well-founded negation interacts subtly with recursion
    * (stratified Datalog), so we keep it out rather than risk leaving it in incorrectly
    */
  def compileBodyClauses(
      conjuncts: Vector[Constraint],
      headRows: Vector[Term.RowRef[?]]
  ): Vector[Q.Clause] = {
    conjuncts.foreach {
      case _: Constraint.Not =>
        throw new RuntimeException(
          "not(...) is not supported inside rule bodies; rewrite to use a positive constraint"
        )
      case _ => ()
    }
    compileClauses(conjuncts, headRows)
  }

  def apply(shape: QueryShape[?]): Output = {
    assertNotPositioning(shape.where, underOr = false)
    validateFind(shape.find)
    val disjuncts = Constraint.dnf(shape.where).map(compileDisjunct(_, shape.find))
    val orderByIdx = shape.orderBys.map { ok =>
      val idx = shape.find.indexWhere(t => termKey(t) == termKey(ok.term))
      if idx < 0 then {
        val printed = printTerm(ok.term)
        throw new RuntimeException(
          s"orderBy term `$printed` does not appear in find(...); add it to the projection"
        )
      }
      (idx, ok.desc)
    }
    Output(disjuncts, shape.find, orderByIdx, shape.limitN, shape.offsetN)
  }

  private def validateFind(find: Vector[Term[?]]): Unit = {
    val hasAgg = find.exists(_.isInstanceOf[Term.Agg[?]])
    val hasWindow = find.exists(_.isInstanceOf[Term.Window[?]])
    if hasAgg && hasWindow then
      throw new RuntimeException(
        "find(...) mixes aggregations and window functions; split into two queries"
      )
    find.foreach {
      case _: Term.Const[?] =>
        throw new RuntimeException("constants are not allowed in find(...)")
      case Term.Agg(_, inner) =>
        inner match {
          case _: Term.Field[?] | _: Term.RowRef[?] => ()
          case _ =>
            throw new RuntimeException(s"aggregation argument must be a field or row, got: $inner")
        }
      case w: Term.Window[?] =>
        w.partition.foreach(requireBindable("partitionBy"))
        w.order.foreach(o => requireBindable("orderBy")(o.term))
        w.args.foreach(requireBindable("window arg"))
      case _ => ()
    }
  }

  private def requireBindable(role: String)(t: Term[?]): Unit = t match {
    case _: Term.Field[?] | _: Term.RowRef[?] => ()
    case _ => throw new RuntimeException(s"$role term must be a field or row, got: $t")
  }

  /** Reject `not(...)` if it sits under `||` or under another `not(...)`.
    *
    * Datalog-safe negation keeps the algebra well-founded... both restrictions are documented on
    * `not` and `Constraint.Not`
    */
  private def assertNotPositioning(c: Constraint, underOr: Boolean): Unit = c match {
    case Constraint.Not(_) if underOr =>
      throw new RuntimeException(
        "not(...) cannot appear inside ||; keep it in the top-level conjunction"
      )
    case Constraint.Not(inner) => assertNoNestedNot(inner)
    case Constraint.And(a, b)  => assertNotPositioning(a, underOr); assertNotPositioning(b, underOr)
    case Constraint.Or(a, b) =>
      assertNotPositioning(a, underOr = true); assertNotPositioning(b, underOr = true)
    case _ => ()
  }

  private def assertNoNestedNot(c: Constraint): Unit = c match {
    case _: Constraint.Not    => throw new RuntimeException("nested not(not(...)) is not supported")
    case Constraint.And(a, b) => assertNoNestedNot(a); assertNoNestedNot(b)
    case Constraint.Or(a, b)  => assertNoNestedNot(a); assertNoNestedNot(b)
    case _                    => ()
  }

  /** Stable structural key for a Term, used to align an `OrderKey` with a `find` column. */
  private def termKey(t: Term[?]): Any = t match {
    case f: Term.Field[?]  => ("field", f.rowBinding, f.attr)
    case r: Term.RowRef[?] => ("row", r.binding)
    case a: Term.Agg[?]    => ("agg", a.fn, termKey(a.inner))
    case c: Term.Const[?]  => ("const", c.value)
  }

  private def printTerm(t: Term[?]): String = t match {
    case f: Term.Field[?]  => f.attr
    case r: Term.RowRef[?] => r.binding
    case a: Term.Agg[?]    => s"${a.fn}(${printTerm(a.inner)})"
    case c: Term.Const[?]  => String.valueOf(c.value)
  }

  /** Deterministic value-var name for a `Term.Field`.
    *
    * Exposed so `TypedSelector` can resolve a field reference to the right key in a binding map
    * without depending on Compile internals
    */
  private[datomlite] def bindField(f: Term.Field[?]): String =
    s"${f.rowBinding}__${f.attr.replace('/', '_')}"

  private def compileDisjunct(
      constraints: Vector[Constraint],
      find: Vector[Term[?]]
  ): Disjunct = {
    val (positives, nots) = constraints.partition {
      case _: Constraint.Not => false
      case _                 => true
    }
    val positive = compileClauses(positives, find)
    val negatives = nots.flatMap {
      case Constraint.Not(inner) =>
        Constraint.dnf(inner).map(sub => compileClauses(sub, Vector.empty))
      case _ => Vector.empty
    }
    Disjunct(positive, negatives)
  }

  /** Lower a flat AND of `Eq` / `Cmp` leaves into a `Q.Clause` vector
    *
    * `find` adds patterns to ensure projected fields are bound... pass `Vector.empty` for negative
    * subgoals so only the inner constraint's own field references contribute patterns
    *
    * Three stages, emitted in order:
    *
    *   1. Index-eligible `Eq` constraints fold into `Q.Pattern`s (AVET probes) 2. Rule
    *      applications, so they run against the row vars the probes pinned rather than
    *      cartesian-joining against later projection patterns 3. `ensureBound` patterns for any
    *      field reference still unbound deferred predicates themselves as `Q.Predicate` filters
    */
  private def compileClauses(
      constraints: Vector[Constraint],
      find: Vector[Term[?]]
  ): Vector[Q.Clause] = {
    val (foldedPatterns, deferred, rules) = partitionConstraints(constraints)

    val ruleClauses = rules.map(lowerRule)

    val bindings = scala.collection.mutable.LinkedHashMap.empty[(String, String), Q.Pattern]
    def ensureBound(t: Term[?]): Unit = t match {
      case f: Term.Field[?] =>
        val key = (f.rowBinding, f.attr)
        if !bindings.contains(key) then
          bindings.update(key, Q.Pattern(Q.Var(f.rowBinding), f.attr, Q.Var(bindField(f))))
      case _ => ()
    }

    deferred.foreach(termsOf(_).foreach(ensureBound))
    find.foreach(termsInFind(_).foreach(ensureBound))

    val predicateClauses = deferred.collect {
      case Constraint.Eq(a, b)      => Q.Predicate(termAsQ(a), Q.Op.Eq, termAsQ(b))
      case Constraint.Cmp(op, a, b) => Q.Predicate(termAsQ(a), op, termAsQ(b))
    }

    foldedPatterns ++ ruleClauses ++ bindings.values ++ predicateClauses
  }

  /** Split constraints into (AVET-foldable patterns, deferred non-pattern leaves, rule apps)
    *
    * The pattern fold handles four shapes... `field === const`, `const === field`, `field ===
    * rowRef`, `rowRef === field`. Everything else lands in `deferred`
    */
  private def partitionConstraints(
      constraints: Vector[Constraint]
  ): (Vector[Q.Clause], Vector[Constraint], Vector[Constraint.RuleApp]) = {
    val patterns = Vector.newBuilder[Q.Clause]
    val deferred = Vector.newBuilder[Constraint]
    val rules = Vector.newBuilder[Constraint.RuleApp]
    constraints.foreach {
      case eq @ Constraint.Eq(lhs, rhs) =>
        tryFoldEq(lhs, rhs).orElse(tryFoldEq(rhs, lhs)) match {
          case Some(p) => patterns += p
          case None    => deferred += eq
        }
      case ra: Constraint.RuleApp => rules += ra
      case other                  => deferred += other
    }
    (patterns.result(), deferred.result(), rules.result())
  }

  private def tryFoldEq(a: Term[?], b: Term[?]): Option[Q.Pattern] = (a, b) match {
    case (f: Term.Field[?], k: Term.Const[?]) =>
      Some(Q.Pattern(Q.Var(f.rowBinding), f.attr, Q.C(k.value)))
    case (f: Term.Field[?], r: Term.RowRef[?]) =>
      Some(Q.Pattern(Q.Var(f.rowBinding), f.attr, Q.Var(r.binding)))
    case _ => None
  }

  private def lowerRule(ra: Constraint.RuleApp): Q.Clause =
    ra.rule.reachesAttr match {
      case Some(attr) => Q.Closure(Q.Var(ra.src.binding), attr, Q.Var(ra.dst.binding))
      case None       => Q.RuleCall(ra.rule, Vector(Q.Var(ra.src.binding), Q.Var(ra.dst.binding)))
    }

  private def termsOf(c: Constraint): Vector[Term[?]] = c match {
    case Constraint.Eq(a, b)     => Vector(a, b)
    case Constraint.Cmp(_, a, b) => Vector(a, b)
    case _                       => Vector.empty
  }

  private def termsInFind(t: Term[?]): Vector[Term[?]] = t match {
    case Term.Agg(_, inner) => Vector(inner)
    case w: Term.Window[?]  => w.args ++ w.partition ++ w.order.map(_.term)
    case other              => Vector(other)
  }

  private def termAsQ(t: Term[?]): Q.Term = t match {
    case f: Term.Field[?]  => Q.Var(bindField(f))
    case r: Term.RowRef[?] => Q.Var(r.binding)
    case c: Term.Const[?]  => Q.C(c.value)
  }
}
