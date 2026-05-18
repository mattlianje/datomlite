package datomlite

import scala.quoted.*

/** A two-place rule, a constraint generator over two row bindings
  *
  * Two flavors live behind the same type:
  *
  *   - `Rule.reaches[A](_.refField)` builds a self-ref closure rule (1+ hops). The runtime walks
  *     the AVET edge directly via `Q.Closure`
  *   - `Rule[A, B](...)` and `Rule.recursive[A, B](self => ...)` build general Datalog rules from
  *     one or more bodies
  *
  * Calling `rule(src, dst)` from inside any `where(...)` lifts to a `Constraint.RuleApp` carrying a
  * reference to the rule itself
  */
final class Rule[A, B](
    private[datomlite] val reachesAttr: Option[String]
) {
  // TODO: revisit this.. but basically we are filling bodies in after construction so
  // a recursive body can reference `this`. Naturally, stays empty for reaches rules and we route
  // to `Q.Closure` instead
  private[datomlite] var bodies: Vector[Rule.Body] = Vector.empty

  /** Apply the rule to two row bindings.
    *
    * Lowers to a `Constraint.RuleApp` that the compiler turns into `Q.Closure` (reaches) or
    * `Q.RuleCall` (defined)
    */
  def apply(src: Term.RowRef[A], dst: Term.RowRef[B]): Constraint =
    Constraint.RuleApp(this, src, dst)
}

object Rule {

  /** A compiled rule body
    *
    * `headVars` are the binding names of the rule's head row params (in positional order);
    * `clauses` is the lowered body. The fact extractor reads the head var positions out of each
    * binding produced by evaluating `clauses`
    */
  final case class Body(headVars: Vector[String], clauses: Vector[Q.Clause])

  /** Transitive closure of a self-ref attribute
    *
    * `r(src, dst)` is true iff `dst` is reachable from `src` in 1 or more hops along `refField`.
    * `f` must be a single-field selector `(_.refField)` where `refField: A` (card-1) or `refField:
    * Set[A]` (card-many)
    *
    * {{{
    *   val reports: Rule[Employee, Employee] = Rule.reaches[Employee](_.manager)
    *
    *   db.query[Employee, Employee] { (e, m) =>
    *     find(e.name, m.name) where reports(e, m)
    *   }.run
    * }}}
    *
    * Performance note... when both row bindings are unbound at the rule's evaluation point, the
    * runtime enumerates the full closure for every source eid (quadratic worst case). Pin one side
    * with an equality first if the dataset is large
    */
  inline def reaches[A](inline f: A => Any)(using e: Entity[A]): Rule[A, A] =
    ${ reachesImpl[A]('f, 'e) }

  /** General multi-body Datalog rule with no `self` reference
    *
    * Each body is a function from the head row params to a `Constraint`; multiple bodies behave
    * like Datalog disjunction (any body matching adds the head pair to the rule's fact set). Use
    * this when you don't need recursion
    *
    * {{{
    *   val canReadDoc: Rule[User, Doc] = Rule[User, Doc](
    *     (u, d) => d.owner === u.email,
    *     (u, d) => d.public === true && u.active === true
    *   )
    * }}}
    */
  def apply[A, B](
      bodies: ((Row[A], Row[B]) => Constraint)*
  )(using ea: Entity[A], eb: Entity[B]): Rule[A, B] = {
    val rule = new Rule[A, B](reachesAttr = None)
    rule.bodies = bodies.toVector.flatMap(compileBody[A, B])
    rule
  }

  /** Recursive Datalog rule.
    *
    * The closure receives the rule itself, so its bodies can call back into it for the recursive
    * cases. The runtime evaluates by naive fixed-point
    *
    * {{{
    *   case class Officer(name: String, boss: Option[String])
    *
    *   val ancestor: Rule[Officer, Officer] =
    *     Rule.recursive[Officer, Officer] { self =>
    *       Seq(
    *         (anc, desc) => desc.boss === anc.name,
    *         (anc, desc) => exists[Officer] { mid =>
    *           desc.boss === mid.name && self(anc, mid)
    *         }
    *       )
    *     }
    * }}}
    *
    * Termination is guaranteed because the fact domain is finite (eids are drawn from the live db).
    * Performance scales with the rule's join shape per iteration; if both head args are unbound at
    * the call site, the runtime materializes the entire fact set
    */
  def recursive[A, B](
      body: Rule[A, B] => Seq[(Row[A], Row[B]) => Constraint]
  )(using ea: Entity[A], eb: Entity[B]): Rule[A, B] = {
    val rule = new Rule[A, B](reachesAttr = None)
    rule.bodies = body(rule).toVector.flatMap(compileBody[A, B])
    rule
  }

  /** Lower a single user-written body into one or more `Rule.Body` entries
    *
    * Disjunctions in the body (`||`) expand via DNF, one body per disjunct, mirroring the top-level
    * query lowering
    */
  private def compileBody[A, B](
      bodyFn: (Row[A], Row[B]) => Constraint
  )(using ea: Entity[A], eb: Entity[B]): Vector[Rule.Body] = {
    val a = freshRow[A]
    val b = freshRow[B]
    val constraint = bodyFn(a, b)
    Constraint.dnf(constraint).map { conjuncts =>
      val clauses = Compile.compileBodyClauses(conjuncts, Vector(a, b))
      Rule.Body(Vector(a.binding, b.binding), clauses)
    }
  }

  /** Macro target for `Rule.reaches`.
    *
    * Extracts the field name from `f` (single-field selector required), then checks at compile time
    * that it's a self-ref on A (either `Ref(A)` or `MultiRef(A)` storage shape). Stores the
    * `"Class/field"` attribute name on the resulting `Rule[A, A]`
    */
  def reachesImpl[A: Type](
      f: Expr[A => Any],
      e: Expr[Entity[A]]
  )(using q: Quotes): Expr[Rule[A, A]] = {
    import q.reflect.*

    def unwrap(t: Term): Term = t match {
      case Inlined(_, _, body) => unwrap(body)
      case Typed(inner, _)     => unwrap(inner)
      case Block(Nil, expr)    => unwrap(expr)
      case _                   => t
    }

    val aRepr = TypeRepr.of[A]
    val aClassName = aRepr.typeSymbol.name
    val aFields = aRepr.typeSymbol.caseFields

    val fieldName: String = unwrap(f.asTerm) match {
      case Lambda(List(_), body) =>
        unwrap(body) match {
          case Select(Ident(_), name) => name
          case other =>
            report.errorAndAbort(
              s"Rule.reaches[$aClassName] expects a single-field selector like `_.field`, got: ${other.show}"
            )
        }
      case other =>
        report.errorAndAbort(
          s"Rule.reaches[$aClassName] expects an inline lambda `(_.field)`, got: ${other.show}"
        )
    }

    val fieldSym = aFields.find(_.name == fieldName).getOrElse {
      val available = aFields.map(_.name).mkString(", ")
      report.errorAndAbort(
        s"`$fieldName` is not a field of $aClassName. Available: $available"
      )
    }

    val fieldType = aRepr.memberType(fieldSym).widen
    val refTarget: TypeRepr = fieldType match {
      case AppliedType(tycon, List(elem)) if tycon =:= TypeRepr.of[Set] => elem
      case t                                                            => t
    }

    if !(refTarget =:= aRepr) then
      report.errorAndAbort(
        s"`$aClassName.$fieldName` is not a self-ref to ${aRepr.show}; Rule.reaches needs a " +
          s"ref (card-1 or card-many) whose target is the same entity so the closure terminates"
      )

    val fieldNameExpr = Expr(fieldName)
    '{ new Rule[A, A](Some($e.name + "/" + $fieldNameExpr)) }
  }
}
