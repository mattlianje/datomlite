package datomlite

import scala.NamedTuple.NamedTuple
import scala.quoted.*

/** Macro implementation backing `find(name = e.name, salary = e.salary)`
  *
  * The Dynamic call desugars each named arg into a `(String, Any)` pair.. this macro recovers the
  * literal label and the underlying `Term[t]` value type so it can return a
  * `QueryShape[NamedTuple[labels, values]]` with full static typing
  */
private[datomlite] object FindMacros {

  /** Positional `find(t1, ..., tN)` lowered through `applyDynamic`
    *
    * Reads each `Term[t]` arg's inner type, then either returns `QueryShape[T]` (arity 1, bare
    * value) or `QueryShape[(T1, ..., TN)]` (arity 2..N, positional tuple)
    */
  def positional(args: Expr[Seq[Any]])(using Quotes): Expr[QueryShape[?]] = {
    import quotes.reflect.*

    val items = args match {
      case Varargs(elts) => elts.toList
      case _ =>
        report.errorAndAbort("find(...) args could not be inspected at compile time")
    }

    if items.isEmpty then report.errorAndAbort("find(...) needs at least one projection")

    val termSym = TypeRepr.of[datomlite.Term[?]].typeSymbol

    def stripWrappers(t: Term): Term = t match {
      case Inlined(_, Nil, inner) => stripWrappers(inner)
      case Typed(inner, _)        => stripWrappers(inner)
      case _                      => t
    }

    final case class Slot(expr: Expr[datomlite.Term[?]], tpe: TypeRepr)

    val slots: List[Slot] = items.zipWithIndex.map { case (item, idx) =>
      val tree = stripWrappers(item.asTerm)
      val tpe = tree.tpe.widen.dealias
      val innerTpe = tpe.baseType(termSym) match {
        case AppliedType(_, List(t)) => t
        case _ =>
          report.errorAndAbort(
            s"find(...) arg at position ${idx + 1} must be a Term[?], got ${tpe.show}",
            item
          )
      }
      Slot(tree.asExprOf[datomlite.Term[?]], innerTpe)
    }

    def mkTuple(ts: List[TypeRepr]): TypeRepr =
      ts.foldRight(TypeRepr.of[EmptyTuple]: TypeRepr) { (h, acc) =>
        (h.asType, acc.asType) match {
          case ('[ht], '[at]) => TypeRepr.of[ht *: (at & Tuple)]
        }
      }

    val termsList = Expr.ofList(slots.map(_.expr))

    if slots.size == 1 then
      slots.head.tpe.asType match {
        case '[t] =>
          '{
            QueryShape[t](
              $termsList.toVector,
              datomlite.Constraint.True,
              isUnary = true
            )
          }
      }
    else
      mkTuple(slots.map(_.tpe)).asType match {
        case '[vt] =>
          '{
            QueryShape[vt & Tuple](
              $termsList.toVector,
              datomlite.Constraint.True,
              isUnary = false
            )
          }
      }
  }

  def named(args: Expr[Seq[(String, Any)]])(using Quotes): Expr[QueryShape[?]] = {
    import quotes.reflect.*

    val pairs = args match {
      case Varargs(elts) => elts.toList
      case _ =>
        report.errorAndAbort("find(...) named args could not be inspected at compile time")
    }

    if pairs.isEmpty then report.errorAndAbort("find(...) needs at least one projection")

    val termSym = TypeRepr.of[datomlite.Term[?]].typeSymbol

    def stripWrappers(t: Term): Term = t match {
      case Inlined(_, Nil, inner) => stripWrappers(inner)
      case Typed(inner, _)        => stripWrappers(inner)
      case _                      => t
    }

    final case class Slot(name: String, expr: Expr[datomlite.Term[?]], tpe: TypeRepr)

    val slots: List[Slot] = pairs.zipWithIndex.map { case (pair, idx) =>
      val tree = stripWrappers(pair.asTerm)
      tree match {
        case Apply(_, List(nameTree, valueTree)) =>
          val name = stripWrappers(nameTree) match {
            case Literal(StringConstant(s)) => s
            case _ =>
              report.errorAndAbort(
                s"find(...) label at position ${idx + 1} must be a literal string",
                pair
              )
          }
          val valueWrapped = stripWrappers(valueTree)
          val valueTpe = valueWrapped.tpe.widen.dealias
          val innerTpe = valueTpe.baseType(termSym) match {
            case AppliedType(_, List(t)) => t
            case _ =>
              report.errorAndAbort(
                s"find(...) `$name` must be a Term[?], got ${valueTpe.show}",
                pair
              )
          }
          Slot(name, valueWrapped.asExprOf[datomlite.Term[?]], innerTpe)
        case _ =>
          report.errorAndAbort(
            s"find(...) could not extract named pair at position ${idx + 1}",
            pair
          )
      }
    }

    val dupes = slots.zipWithIndex.groupBy(_._1.name).filter(_._2.size > 1)
    if dupes.nonEmpty then {
      val msg = dupes.iterator.map { case (n, occs) =>
        s"`$n` at positions ${occs.map(_._2 + 1).mkString(", ")}"
      }.mkString("; ")
      report.errorAndAbort(s"find(...) has duplicate labels: $msg")
    }

    def mkTuple(ts: List[TypeRepr]): TypeRepr =
      ts.foldRight(TypeRepr.of[EmptyTuple]: TypeRepr) { (h, acc) =>
        (h.asType, acc.asType) match {
          case ('[ht], '[at]) => TypeRepr.of[ht *: (at & Tuple)]
        }
      }

    val labelsTuple = mkTuple(slots.map(s => ConstantType(StringConstant(s.name)): TypeRepr))
    val valuesTuple = mkTuple(slots.map(_.tpe))

    (labelsTuple.asType, valuesTuple.asType) match {
      case ('[lt], '[vt]) =>
        Type.of[NamedTuple[lt & Tuple, vt & Tuple]] match {
          case '[ntType] =>
            val termsList = Expr.ofList(slots.map(_.expr))
            '{
              QueryShape[ntType](
                $termsList.toVector,
                datomlite.Constraint.True,
                isUnary = false
              )
            }
        }
    }
  }
}
