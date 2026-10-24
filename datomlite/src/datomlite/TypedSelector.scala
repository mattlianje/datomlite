package datomlite

/** Result of running a typed query
  *
  * `T` is the projected row type from `find(...)`, bare value for arity 1, tuple for arity 2+, so
  * `.run` produces `Vector[T]` with no cast at the call site
  */
final class TypedSelector[T] private[datomlite] (
    db: Db,
    disjuncts: Vector[Compile.Disjunct],
    finds: Vector[Term[?]],
    isUnary: Boolean,
    orderByIdx: Vector[(Int, Boolean)] = Vector.empty,
    limitN: Option[Int] = None,
    offsetN: Int = 0,
    dedup: Boolean = false
) extends Iterable[T] {

  /** Typed projection
    *
    * Each row is a `T` aligned with the find list. With `||`, the constraint compiles to multiple
    * disjunct clause vectors.. each runs independently and the bindings are concatenated before
    * projection
    */
  def run: Vector[T] = {
    val bindings = disjuncts.flatMap { d =>
      val pos = db.internal.evalClauses(d.positive).toVector
      if d.negatives.isEmpty then pos
      else
        pos.filter { binding =>
          d.negatives.forall { neg =>
            db.internal.evalClauses(Q.substituteClauses(neg, binding)).isEmpty
          }
        }
    }
    val hasAgg = finds.exists(_.isInstanceOf[Term.Agg[?]])
    val hasWindow = finds.exists(_.isInstanceOf[Term.Window[?]])
    val raw: Vector[Vector[Any]] =
      if hasWindow then projectWithWindows(bindings)
      else if hasAgg then projectWithAgg(bindings)
      else projectFlat(bindings)
    val deduped = if dedup then raw.distinct else raw
    val sorted = if orderByIdx.isEmpty then deduped else sortRows(deduped)
    val sliced = {
      val afterOffset = if offsetN > 0 then sorted.drop(offsetN) else sorted
      limitN.fold(afterOffset)(afterOffset.take)
    }
    if isUnary then sliced.map(_.head.asInstanceOf[T])
    else sliced.map(row => Tuple.fromArray(row.toArray).asInstanceOf[T])
  }

  private def projectFlat(bindings: Vector[Map[String, Any]]): Vector[Vector[Any]] =
    bindings.map(b => finds.map(t => readScalar(t, b)))

  private def projectWithAgg(bindings: Vector[Map[String, Any]]): Vector[Vector[Any]] = {
    val groupTerms = finds.collect { case t: Term.Field[?] => t; case t: Term.RowRef[?] => t }
    val groupKeys = groupTerms.map(varNameOf)
    val grouped = bindings.groupBy(b => groupKeys.map(k => b.getOrElse(k, null)))
    grouped.iterator.map { (key, group) =>
      finds.map {
        case Term.Agg(fn, inner) =>
          Q.aggregate(fn, group.map(_.getOrElse(varNameOf(inner), null)))
        case t =>
          key(groupTerms.indexOf(t))
      }
    }.toVector
  }

  private def projectWithWindows(bindings: Vector[Map[String, Any]]): Vector[Vector[Any]] = {
    val windowCols: Map[Int, Vector[Any]] = finds.zipWithIndex.collect {
      case (w: Term.Window[?], idx) => idx -> TypedSelector.computeWindowColumn(bindings, w)
    }.toMap
    bindings.indices.toVector.map { rowIdx =>
      finds.zipWithIndex.map { (term, colIdx) =>
        term match {
          case _: Term.Window[?] => windowCols(colIdx)(rowIdx)
          case other             => readScalar(other, bindings(rowIdx))
        }
      }
    }
  }

  private def readScalar(t: Term[?], b: Map[String, Any]): Any = t match {
    case f: Term.Field[?] =>
      b.getOrElse(Compile.bindField(f), throw new RuntimeException(s"unbound field: ${f.attr}"))
    case r: Term.RowRef[?] =>
      b.getOrElse(r.binding, throw new RuntimeException(s"unbound row: ${r.binding}"))
    case _: Term.Agg[?]    => sys.error("agg projected via flat path; should be in agg path")
    case _: Term.Window[?] => sys.error("window projected via flat path; should be in window path")
    case _: Term.Const[?]  => sys.error("constants are not allowed in find(...)")
  }

  private def varNameOf(t: Term[?]): String = t match {
    case f: Term.Field[?]  => Compile.bindField(f)
    case r: Term.RowRef[?] => r.binding
    case _                 => sys.error(s"expected field/row, got: $t")
  }

  private def sortRows(rows: Vector[Vector[Any]]): Vector[Vector[Any]] =
    rows.sortWith { (a, b) =>
      TypedSelector.lexLess(orderByIdx.iterator.map { (idx, desc) =>
        (TypedSelector.compareAny(a(idx), b(idx)), desc)
      })
    }

  def iterator: Iterator[T] = run.iterator

  /** At most one row, taken from the head of the result. Same as `headOption`. */
  def one: Option[T] = run.headOption

  /** Distinct rows
    *
    * Datalog convention is set semantics.. call `.distinct` when joining or when disjuncts overlap
    */
  def distinct: TypedSelector[T] =
    new TypedSelector[T](db, disjuncts, finds, isUnary, orderByIdx, limitN, offsetN, dedup = true)
}

private[datomlite] object TypedSelector {

  /** Compute one column of window-function values for the given window term
    *
    * Indexed by binding position. Partitions by the window's partition keys, sorts each partition
    * by `order`, then fills per-row values via `windowKernel`
    */
  def computeWindowColumn(
      bindings: Vector[Map[String, Any]],
      w: Term.Window[?]
  ): Vector[Any] = {
    val partKeys = w.partition.map(varName)
    val partitions: Map[Vector[Any], Vector[Int]] =
      bindings.indices.toVector.groupBy(idx =>
        partKeys.map(k => bindings(idx).getOrElse(k, null))
      )
    val out = Array.ofDim[Any](bindings.size)
    partitions.foreach { case (_, idxs) =>
      val sorted =
        if w.order.isEmpty then idxs
        else idxs.sortWith((a, b) => compareByOrderKeys(bindings(a), bindings(b), w.order))
      windowKernel(out, sorted, bindings, w)
    }
    out.toVector
  }

  private def varName(t: Term[?]): String = t match {
    case f: Term.Field[?]  => Compile.bindField(f)
    case r: Term.RowRef[?] => r.binding
    case _                 => sys.error(s"expected field/row, got: $t")
  }

  private def compareByOrderKeys(
      a: Map[String, Any],
      b: Map[String, Any],
      keys: Vector[OrderKey]
  ): Boolean =
    lexLess(keys.iterator.map { k =>
      val name = varName(k.term)
      (compareAny(a.getOrElse(name, null), b.getOrElse(name, null)), k.desc)
    })

  /** Lexicographic less-than over a stream of `(cmp, desc)` steps
    *
    * Stops at the first non-zero comparison and flips the sign for descending keys. Used by both
    * row sorting and per-partition window ordering
    */
  private[datomlite] def lexLess(steps: Iterator[(Int, Boolean)]): Boolean =
    steps.find((cmp, _) => cmp != 0).exists((cmp, desc) => if desc then cmp > 0 else cmp < 0)

  private def windowKernel(
      out: Array[Any],
      sorted: Vector[Int],
      bindings: Vector[Map[String, Any]],
      w: Term.Window[?]
  ): Unit = {
    def keyOf(origIdx: Int): Vector[Any] =
      w.order.map(o => bindings(origIdx).getOrElse(varName(o.term), null))
    w.fn match {
      case "row_number" =>
        sorted.zipWithIndex.foreach { case (origIdx, pos) => out(origIdx) = (pos + 1).toLong }
      case "rank" =>
        sorted.zipWithIndex.foldLeft((0L, Option.empty[Vector[Any]])) {
          case ((rank, prev), (origIdx, i)) =>
            val k = keyOf(origIdx)
            val nextRank = if prev.contains(k) then rank else (i + 1).toLong
            out(origIdx) = nextRank
            (nextRank, Some(k))
        }
      case "dense_rank" =>
        sorted.foldLeft((0L, Option.empty[Vector[Any]])) { case ((rank, prev), origIdx) =>
          val k = keyOf(origIdx)
          val nextRank = if prev.contains(k) then rank else rank + 1
          out(origIdx) = nextRank
          (nextRank, Some(k))
        }
      case s"lag:$nStr" =>
        val n = nStr.toInt
        val arg = varName(w.args.head)
        sorted.zipWithIndex.foreach { case (origIdx, pos) =>
          val src = pos - n
          out(origIdx) =
            if src < 0 || src >= sorted.size then None
            else Some(bindings(sorted(src)).getOrElse(arg, null))
        }
      case s"lead:$nStr" =>
        val n = nStr.toInt
        val arg = varName(w.args.head)
        sorted.zipWithIndex.foreach { case (origIdx, pos) =>
          val src = pos + n
          out(origIdx) =
            if src < 0 || src >= sorted.size then None
            else Some(bindings(sorted(src)).getOrElse(arg, null))
        }
      case s"agg/$inner" =>
        val arg = varName(w.args.head)
        val values = sorted.map(idx => bindings(idx).getOrElse(arg, null))
        val total = Q.aggregate(inner, values)
        sorted.foreach(origIdx => out(origIdx) = total)
      case other =>
        sys.error(s"unknown window function: $other")
    }
  }

  /** Cross-type comparator used by `sortRows`
    *
    * Forwards to the shared `Compare.any` so row sorting widens numbers identically to predicate
    * evaluation
    */
  def compareAny(a: Any, b: Any): Int = Compare.any(a, b)
}

extension (db: Db) {

  /** Typed query, single row binding
    *
    * The entity types travel as type arguments so the lambda params can be plain `e` instead of `e:
    * Row[Employee]`
    *
    * {{{
    *    db.query[Employee] { e =>
    *      find(e.email) where (e.salary > 100_000L)
    *    }.run   // Vector[String]
    * }}}
    */
  def query[A](using ea: Entity[A]): QueryBuilder1[A] = new QueryBuilder1[A](db)

  /** Two-row binding (the common join shape). */
  def query[A, B](using ea: Entity[A], eb: Entity[B]): QueryBuilder2[A, B] =
    new QueryBuilder2[A, B](db)

  /** Three-row binding (covers most multi-join queries). */
  def query[A, B, C](using
      ea: Entity[A],
      eb: Entity[B],
      ec: Entity[C]
  ): QueryBuilder3[A, B, C] = new QueryBuilder3[A, B, C](db)

  /** Four-row binding. */
  def query[A, B, C, D](using
      ea: Entity[A],
      eb: Entity[B],
      ec: Entity[C],
      ed: Entity[D]
  ): QueryBuilder4[A, B, C, D] = new QueryBuilder4[A, B, C, D](db)

  /** Five-row binding. */
  def query[A, B, C, D, E](using
      ea: Entity[A],
      eb: Entity[B],
      ec: Entity[C],
      ed: Entity[D],
      ee: Entity[E]
  ): QueryBuilder5[A, B, C, D, E] = new QueryBuilder5[A, B, C, D, E](db)
}

/** Curried builder so `query[A]` only fixes the entity type
  */
final class QueryBuilder1[A] private[datomlite] (db: Db)(using ea: Entity[A]) {
  def apply[T](f: Row[A] => QueryShape[T]): TypedSelector[T] =
    runShape(db, f(freshRow[A]))
}

final class QueryBuilder2[A, B] private[datomlite] (db: Db)(using ea: Entity[A], eb: Entity[B]) {
  def apply[T](f: (Row[A], Row[B]) => QueryShape[T]): TypedSelector[T] =
    runShape(db, f(freshRow[A], freshRow[B]))
}

final class QueryBuilder3[A, B, C] private[datomlite] (db: Db)(using
    ea: Entity[A],
    eb: Entity[B],
    ec: Entity[C]
) {
  def apply[T](f: (Row[A], Row[B], Row[C]) => QueryShape[T]): TypedSelector[T] =
    runShape(db, f(freshRow[A], freshRow[B], freshRow[C]))
}

final class QueryBuilder4[A, B, C, D] private[datomlite] (db: Db)(using
    ea: Entity[A],
    eb: Entity[B],
    ec: Entity[C],
    ed: Entity[D]
) {
  def apply[T](f: (Row[A], Row[B], Row[C], Row[D]) => QueryShape[T]): TypedSelector[T] =
    runShape(db, f(freshRow[A], freshRow[B], freshRow[C], freshRow[D]))
}

final class QueryBuilder5[A, B, C, D, E] private[datomlite] (db: Db)(using
    ea: Entity[A],
    eb: Entity[B],
    ec: Entity[C],
    ed: Entity[D],
    ee: Entity[E]
) {
  def apply[T](
      f: (Row[A], Row[B], Row[C], Row[D], Row[E]) => QueryShape[T]
  ): TypedSelector[T] =
    runShape(db, f(freshRow[A], freshRow[B], freshRow[C], freshRow[D], freshRow[E]))
}

private[datomlite] def runShape[T](db: Db, shape: QueryShape[T]): TypedSelector[T] = {
  val out = Compile(shape)
  new TypedSelector[T](
    db,
    out.disjuncts,
    out.finds,
    shape.isUnary,
    out.orderByIdx,
    out.limitN,
    out.offsetN
  )
}
