package datomlite

import java.util.concurrent.atomic.AtomicReference

private[datomlite] object DbImpl {

  /** Widen any `Entity[A]` to the internal `Entity[Any]` form. The runtime stores values as `Any`,
    * so internal helpers all take teh widened form
    */
  private inline def widen[A](e: Entity[A]): Entity[Any] = e.asInstanceOf[Entity[Any]]

  final case class State(
      nextEid: Long,
      nextTxId: Long,
      /** eid -> (attr -> set of values) */
      eavt: Map[Eid, Map[String, Set[Any]]],
      /** entity name -> live eids */
      eidByClass: Map[String, Set[Eid]],
      /** attr -> value -> eids (hash index) */
      avet: Map[String, Map[Any, Set[Eid]]],
      log: Vector[Datom]
  ) {
    def commit(tx: Tx, time: Time): (State, TxReport) = {
      val txId = TxId(nextTxId)
      val acc = applyTx(tx, txId, time)
      (
        acc.state.copy(nextTxId = nextTxId + 1),
        TxReport(txId, time, acc.adds, acc.retracts, acc.eids)
      )
    }

    /** Single-step apply...
      *
      * Threads State and accumulates report fields. Sub-steps of `Tx.Batch` share the caller's
      * `txId` and `time`, so the whole block lands as one transaction
      */
    private def applyTx(tx: Tx, txId: TxId, time: Time): TxAccum = tx match {
      case Tx.Add(vs)    => assertMany(vs, txId, time, upsert = false)
      case Tx.Upsert(vs) => assertMany(vs, txId, time, upsert = true)
      case Tx.Retract(vs) =>
        val s = vs.foldLeft(this)((st, d) => st.retractEntity(d, txId, time))
        TxAccum(s, Vector.empty, vs, Vector.empty)
      case Tx.Batch(steps) =>
        steps.foldLeft(TxAccum.empty(this)) { (acc, step) =>
          acc.combine(acc.state.applyTx(step, txId, time))
        }
    }

    private def assertMany(
        vs: Vector[Datum],
        txId: TxId,
        time: Time,
        upsert: Boolean
    ): TxAccum = {
      val (s, eids) = vs.foldLeft((this, Vector.empty[Long])) { case ((st, acc), d) =>
        val (st2, eid) = st.assertEntityRec(d, txId, time, upsert)
        (st2, acc :+ Eid.value(eid))
      }
      TxAccum(s, vs, Vector.empty, eids)
    }

    /** Recursive assert that threads `State` through nested refs.
      *
      * Returns the (possibly unchanged) state and the eid this datum resolves to. Handles all four
      * shapes: Value, MultiValue, Ref (recurses), MultiRef (recurses per element)
      *
      * `upsert = true` flips the key-conflict path... instead of throwing `UniqueViolation` on a
      * `.key` collision, the colliding eid's datoms are retracted and the new value re-asserted
      * under the same eid. `.unique`-but-not-key collisions still throw. Nested ref asserts always
      * run in non-upsert mode; upsert is shallow on the top-level entity
      */
    def assertEntityRec(
        d: Datum,
        tx: TxId,
        time: Time,
        upsert: Boolean = false
    ): (State, Eid) = {
      val entity = d.entity

      /** Walk the entity's fields...
        *
        * Threads State through nested ref asserts and accumulates the flat (attr, value) pairs.
        * Refs and MultiRefs may grow the state; scalar shapes don't
        */
      val (s, flat) = fieldActions(entity, d.value)
        .foldLeft((this, Vector.empty[(String, Any)])) {
          case ((st, acc), FieldAction.Plain(pairs)) =>
            (st, acc ++ pairs)
          case ((st, acc), FieldAction.Ref(attr, target, value)) =>
            val (st2, targetEid) = st.assertEntityRec(Datum(value, target), tx, time)
            (st2, acc :+ (attr -> Eid.value(targetEid)))
          case ((st, acc), FieldAction.MultiRef(attr, target, values)) =>
            values.foldLeft((st, acc)) { case ((st3, acc3), nested) =>
              val (st4, targetEid) = st3.assertEntityRec(Datum(nested, target), tx, time)
              (st4, acc3 :+ (attr -> Eid.value(targetEid)))
            }
        }

      val attrsAsSets: Map[String, Set[Any]] = State.collapseToSets(flat)

      s.findExisting(entity.name, attrsAsSets) match {
        case Some(eid) => (s, eid)
        case None =>
          val live = s.eidByClass.getOrElse(entity.name, Set.empty)

          val keyMatchEid: Option[Eid] =
            if !upsert then None
            else
              entity.keyOf.flatMap { keyFn =>
                val newKey = keyFn(d.value)
                live.find { eid =>
                  val existing = reconstructEntity(s, eid, widen(entity))
                  keyFn(existing) == newKey
                }
              }

          val constraints: List[(String, Any => Any)] =
            entity.keyOf.toList.map("key" -> _) ::: entity.uniques.map("unique" -> _)
          if constraints.nonEmpty then
            constraints.foreach { case (kind, fn) =>
              val newKey = fn(d.value)
              live.foreach { eid =>
                if !keyMatchEid.contains(eid) then {
                  val existing = reconstructEntity(s, eid, widen(entity))
                  if fn(existing) == newKey then
                    throw new UniqueViolation(entity.name, kind, newKey)
                }
              }
            }

          keyMatchEid match {
            case Some(existingEid) =>
              val oldFlat = flatten(s.eavt.getOrElse(existingEid, Map.empty))
              val retractDatoms =
                oldFlat.map((a, vv) => Datom(existingEid, a, vv, tx, time, Op.Retract))
              val assertDatoms = flat.map((a, vv) => Datom(existingEid, a, vv, tx, time, Op.Assert))
              val newState = s.copy(
                eavt = s.eavt.updated(existingEid, attrsAsSets),
                avet =
                  State.avetAdd(State.avetRemove(s.avet, existingEid, oldFlat), existingEid, flat),
                log = s.log ++ retractDatoms ++ assertDatoms
              )
              (newState, existingEid)
            case None =>
              val newEid = Eid(s.nextEid)
              val datoms = flat.map((a, vv) => Datom(newEid, a, vv, tx, time, Op.Assert))
              val newState = s.copy(
                nextEid = s.nextEid + 1,
                eavt = s.eavt.updated(newEid, attrsAsSets),
                eidByClass = s.eidByClass.updated(
                  entity.name,
                  s.eidByClass.getOrElse(entity.name, Set.empty) + newEid
                ),
                avet = State.avetAdd(s.avet, newEid, flat),
                log = s.log ++ datoms
              )
              (newState, newEid)
          }
      }
    }

    def retractEntity(d: Datum, tx: TxId, time: Time): State = {
      val entity = d.entity
      flattenForLookup(this, entity, d.value) match {
        case None => this
        case Some(flat) =>
          val attrsAsSets = State.collapseToSets(flat)
          findExisting(entity.name, attrsAsSets) match {
            case None => this
            case Some(eid) =>
              val datoms = flat.map((a, vv) => Datom(eid, a, vv, tx, time, Op.Retract))
              copy(
                eavt = eavt - eid,
                eidByClass = eidByClass.updated(
                  entity.name,
                  eidByClass.getOrElse(entity.name, Set.empty) - eid
                ),
                avet = State.avetRemove(avet, eid, flatten(eavt.getOrElse(eid, Map.empty))),
                log = log ++ datoms
              )
          }
      }
    }

    private def findExisting(className: String, attrs: Map[String, Set[Any]]): Option[Eid] =
      eidByClass.getOrElse(className, Set.empty).find(eid => eavt.get(eid).contains(attrs))

    def lookup(attr: String, value: Any): Set[Eid] =
      avet.getOrElse(attr, Map.empty).getOrElse(value, Set.empty)
  }

  /** Accumulator threaded through `applyTx`.
    *
    * Holds the post-step `State` plus the report fields a `Tx.Batch` keeps growing as it walks each
    * sub-step. `combine` keeps the right-hand state and concatenates the report vectors, mirroring
    * sequential application order
    */
  final case class TxAccum(
      state: State,
      adds: Vector[Datum],
      retracts: Vector[Datum],
      eids: Vector[Long]
  ) {
    def combine(next: TxAccum): TxAccum =
      TxAccum(next.state, adds ++ next.adds, retracts ++ next.retracts, eids ++ next.eids)
  }

  object TxAccum {
    def empty(s: State): TxAccum = TxAccum(s, Vector.empty, Vector.empty, Vector.empty)
  }

  object State {
    val empty: State = State(0L, 0L, Map.empty, Map.empty, Map.empty, Vector.empty)

    def collapseToSets(flat: Iterable[(String, Any)]): Map[String, Set[Any]] =
      flat.foldLeft(Map.empty[String, Set[Any]]) { case (acc, (k, v)) =>
        acc.updated(k, acc.getOrElse(k, Set.empty) + v)
      }

    def avetAdd(
        avet: Map[String, Map[Any, Set[Eid]]],
        eid: Eid,
        attrs: Vector[(String, Any)]
    ): Map[String, Map[Any, Set[Eid]]] =
      attrs.foldLeft(avet) { case (acc, (a, v)) =>
        val byVal = acc.getOrElse(a, Map.empty)
        val eidsNow = byVal.getOrElse(v, Set.empty) + eid
        acc.updated(a, byVal.updated(v, eidsNow))
      }

    def avetRemove(
        avet: Map[String, Map[Any, Set[Eid]]],
        eid: Eid,
        attrs: Vector[(String, Any)]
    ): Map[String, Map[Any, Set[Eid]]] =
      attrs.foldLeft(avet) { case (acc, (a, v)) =>
        val byVal = acc.getOrElse(a, Map.empty)
        val left = byVal.getOrElse(v, Set.empty) - eid
        val byVal2 = if left.isEmpty then byVal - v else byVal.updated(v, left)
        if byVal2.isEmpty then acc - a else acc.updated(a, byVal2)
      }

    def rebuild(log: Vector[Datom]): State = {
      val byOriginalEid = log.groupBy(_.e)
      val live = byOriginalEid.collect {
        case (origEid, entries) =>
          val finalAttrs = entries.foldLeft(Map.empty[String, Set[Any]]) { (acc, d) =>
            d.op match {
              case Op.Assert =>
                acc.updated(d.a, acc.getOrElse(d.a, Set.empty) + d.v)
              case Op.Retract =>
                val left = acc.getOrElse(d.a, Set.empty) - d.v
                if left.isEmpty then acc - d.a else acc.updated(d.a, left)
            }
          }
          (origEid, finalAttrs)
      }.filter(_._2.nonEmpty)

      val eidByClass = live.toVector.flatMap { (eid, attrs) =>
        attrs.keys.headOption.map(a => a.takeWhile(_ != '/') -> eid)
      }.groupMap(_._1)(_._2).view.mapValues(_.toSet).toMap

      val avet = live.foldLeft(Map.empty[String, Map[Any, Set[Eid]]]) {
        case (acc, (eid, attrs)) => avetAdd(acc, eid, flatten(attrs))
      }

      val nextEid: Long = live.keys.iterator.map(Eid.value(_)).maxOption.getOrElse(-1L) + 1L
      val nextTxId: Long = log.iterator.map(_.t.value).maxOption.getOrElse(-1L) + 1L
      State(nextEid, nextTxId, live, eidByClass, avet, log)
    }
  }

  /** State-aware reconstruct that follows Ref / MultiRef pointers. */
  private def reconstructEntity[A](
      state: State,
      eid: Eid,
      e: Entity[A],
      seen: Set[Eid] = Set.empty
  ): A = {
    if seen(eid) then
      throw new RuntimeException(
        s"cycle detected at eid ${Eid.value(eid)} reconstructing ${e.name}"
      )
    val attrs = state.eavt.getOrElse(eid, Map.empty)
    val prefix = s"${e.name}/"
    val materialized: Map[String, Any] =
      e.fields.zip(e.shapes).map { case (field, shape) =>
        val attrKey = prefix + field
        val values = attrs.getOrElse(attrKey, Set.empty[Any])
        val v: Any = shape match {
          case FieldShape.Value =>
            values.head
          case FieldShape.MultiValue =>
            values
          case FieldShape.Optional =>
            if values.isEmpty then None else Some(values.head)
          case FieldShape.Ref(targetFn) =>
            val target = targetFn()
            val targetEid = Eid(values.head.asInstanceOf[Long])
            reconstructEntity(state, targetEid, target, seen + eid)
          case FieldShape.MultiRef(targetFn) =>
            val target = targetFn()
            values.map { vv =>
              val targetEid = Eid(vv.asInstanceOf[Long])
              reconstructEntity(state, targetEid, target, seen + eid)
            }
        }
        field -> v
      }.toMap
    e.reconstruct(materialized)
  }

  /** Find an existing eid for `value` of type `entity` by structural match.
    *
    * Used by retract to resolve refs without inserting
    */
  private def findEidFor(state: State, entity: Entity[Any], value: Any): Option[Eid] =
    flattenForLookup(state, entity, value).flatMap { flat =>
      val attrsAsSets = State.collapseToSets(flat)
      state.eidByClass.getOrElse(entity.name, Set.empty).find { eid =>
        state.eavt.get(eid).contains(attrsAsSets)
      }
    }

  /** Decompose `value` into its flat (attr, value) pairs against `state` for read-only lookup.
    *
    * Refs are resolved through `findEidFor`; if any nested ref doesn't currently exist in `state`,
    * the whole entity is unresolvable and we return `None`. Distinct from `assertEntityRec`, which
    * inserts as it goes
    */
  private def flattenForLookup(
      state: State,
      entity: Entity[Any],
      value: Any
  ): Option[Vector[(String, Any)]] =
    fieldActions(entity, value).foldLeft(Option(Vector.empty[(String, Any)])) {
      case (None, _) => None
      case (Some(acc), FieldAction.Plain(pairs)) =>
        Some(acc ++ pairs)
      case (Some(acc), FieldAction.Ref(attr, target, v)) =>
        findEidFor(state, target, v).map(eid => acc :+ (attr -> Eid.value(eid)))
      case (Some(acc), FieldAction.MultiRef(attr, target, values)) =>
        values.foldLeft(Option(acc)) {
          case (None, _) => None
          case (Some(a), nested) =>
            findEidFor(state, target, nested).map(eid => a :+ (attr -> Eid.value(eid)))
        }
    }

  /** Per-field decision for the shape walk.
    *
    * `Plain` is the static case... Value, MultiValue, Optional all collapse into a vector of (attr,
    * value) pairs that the caller drops onto its accumulator unchanged. `Ref` and `MultiRef` hand
    * the caller the target entity and the nested value(s) so `assertEntityRec` (which inserts) and
    * `flattenForLookup` (which probes) can each do their own thing without duplicating the
    * per-shape switch
    */
  sealed private trait FieldAction
  private object FieldAction {
    final case class Plain(pairs: Vector[(String, Any)]) extends FieldAction
    final case class Ref(attr: String, target: Entity[Any], value: Any) extends FieldAction
    final case class MultiRef(attr: String, target: Entity[Any], values: Set[Any])
        extends FieldAction
  }

  /** One walk over `entity.shapes` that yields a `FieldAction` per field.
    *
    * Both the assert and the lookup paths fold over this iterator; the per-shape decoding lives
    * here once
    */
  private def fieldActions(entity: Entity[Any], value: Any): Iterator[FieldAction] = {
    val prefix = s"${entity.name}/"
    entity.decompose(value).iterator.zip(entity.shapes.iterator).map { case ((field, v), shape) =>
      val attr = prefix + field
      shape match {
        case FieldShape.Value =>
          FieldAction.Plain(Vector(attr -> v))
        case FieldShape.MultiValue =>
          FieldAction.Plain(v.asInstanceOf[Set[Any]].iterator.map(attr -> _).toVector)
        case FieldShape.Optional =>
          FieldAction.Plain(v.asInstanceOf[Option[Any]].toVector.map(attr -> _))
        case FieldShape.Ref(targetFn) =>
          FieldAction.Ref(attr, widen(targetFn()), v)
        case FieldShape.MultiRef(targetFn) =>
          FieldAction.MultiRef(attr, widen(targetFn()), v.asInstanceOf[Set[Any]])
      }
    }
  }

  /** Flatten an `eavt` row (attr -> set of values) into the (attr, value) tuples used by AVET
    * upkeep and history rebuilds.
    */
  private def flatten(attrs: Map[String, Set[Any]]): Vector[(String, Any)] =
    attrs.iterator.flatMap { case (k, vs) => vs.iterator.map(k -> _) }.toVector

  private def eidLookup[A](state: State, eid: Long, e: Entity[A]): Option[A] = {
    val eidT = Eid(eid)
    if state.eidByClass.getOrElse(e.name, Set.empty).contains(eidT) then
      Some(reconstructEntity(state, eidT, e))
    else None
  }

  private def reconstructAll[A](state: State, e: Entity[A]): Iterator[A] =
    state.eidByClass.getOrElse(e.name, Set.empty).iterator
      .map(eid => reconstructEntity(state, eid, e))

  private def reconstructEids[A](state: State, eids: Set[Eid], e: Entity[A]): Iterator[A] =
    eids.iterator.map(eid => reconstructEntity(state, eid, e))

  private def byEqQuery[A](
      db: Db,
      state: State,
      e: Entity[A],
      attr: String,
      value: Any
  ): Query[A] = {
    val key = s"${e.name}/$attr"
    val eids = state.lookup(key, value)
    Query(db, () => reconstructEids(state, eids, e))
  }

  private def evalClausesAt(state: State, clauses: Vector[Q.Clause]): Iterator[Map[String, Any]] = {
    val registry = {
      val refs = collectRules(clauses, Set.empty)
      if refs.isEmpty then Map.empty[AnyRef, Set[Vector[Long]]]
      else evaluateRules(refs, state)
    }
    runClauses(state, clauses, registry)
  }

  private def runClauses(
      state: State,
      clauses: Vector[Q.Clause],
      registry: Map[AnyRef, Set[Vector[Long]]]
  ): Iterator[Map[String, Any]] =
    clauses.foldLeft(Iterator.single(Map.empty[String, Any])) { (rows, c) =>
      c match {
        case p: Q.Pattern   => rows.flatMap(row => matchPattern(state, p, row))
        case p: Q.Predicate => rows.filter(row => evalPredicate(p, row))
        case c: Q.Closure   => rows.flatMap(row => matchClosure(state, c, row))
        case r: Q.RuleCall  => rows.flatMap(row => matchRuleCall(registry, r, row))
      }
    }

  /** Walk the clause vector and collect every rule referenced, transitively through other rules'
    * bodies.
    *
    * The result is the closed set of rules whose facts must be computed before this query can be
    * evaluated
    */
  private def collectRules(
      clauses: Vector[Q.Clause],
      acc: Set[AnyRef]
  ): Set[AnyRef] =
    clauses.foldLeft(acc) { (a, c) =>
      c match {
        case rc: Q.RuleCall if !a.contains(rc.rule) =>
          val a2 = a + rc.rule
          val rule = rc.rule.asInstanceOf[datomlite.Rule[?, ?]]
          rule.bodies.foldLeft(a2)((acc2, body) => collectRules(body.clauses, acc2))
        case _ => a
      }
    }

  /** Naive fixed-point evaluator over the closed rule set.
    *
    * Iterates until no rule's fact set grows. Each iteration re-evaluates every body of every rule
    * against the current registry; new facts feed the next round. Terminates because the fact
    * universe is finite (each fact is a tuple of eids drawn from the live db)
    *
    * Naive rather than semi-naive... clearer to read, and the redundant work hasn't shown up in
    * profiles yet. Swap in a delta-driven evaluator if rule-heavy workloads ever do
    */
  private def evaluateRules(
      rules: Set[AnyRef],
      state: State
  ): Map[AnyRef, Set[Vector[Long]]] = {
    var current: Map[AnyRef, Set[Vector[Long]]] =
      rules.iterator.map(r => r -> Set.empty[Vector[Long]]).toMap
    var changed = true
    while changed do {
      changed = false
      rules.foreach { rRef =>
        val rule = rRef.asInstanceOf[datomlite.Rule[?, ?]]
        val produced: Set[Vector[Long]] = rule.bodies.iterator.flatMap { body =>
          runClauses(state, body.clauses, current).flatMap { binding =>
            val tuple = body.headVars.map { name =>
              binding.get(name) match {
                case Some(v: Long) => Some(v)
                case Some(v: Int)  => Some(v.toLong)
                case _             => None
              }
            }
            if tuple.forall(_.isDefined) then Iterator.single(tuple.map(_.get))
            else Iterator.empty
          }
        }.toSet
        if produced != current(rRef) then {
          current = current.updated(rRef, produced)
          changed = true
        }
      }
    }
    current
  }

  /** Bind the call's arg vars from each known fact for the called rule.
    *
    * A `Var` arg binds (or filters, if already bound to a different value); a `C` arg behaves as a
    * literal filter. The inner loop short-circuits on the first failed binding, so it stays
    * imperative on purpose
    */
  private def matchRuleCall(
      registry: Map[AnyRef, Set[Vector[Long]]],
      rc: Q.RuleCall,
      row: Map[String, Any]
  ): Iterator[Map[String, Any]] = {
    val facts = registry.getOrElse(rc.rule, Set.empty)
    facts.iterator.flatMap { fact =>
      var current = row
      var ok = true
      var i = 0
      while ok && i < rc.args.length do {
        val arg = rc.args(i)
        val factVal = fact(i)
        arg match {
          case Q.Var(n) =>
            current.get(n) match {
              case Some(existing) =>
                val asLong = existing match {
                  case l: Long => l
                  case i: Int  => i.toLong
                  case _       => -1L
                }
                if asLong != factVal then ok = false
              case None =>
                current = current.updated(n, factVal)
            }
          case Q.C(c) =>
            val asLong = c match {
              case l: Long => l
              case i: Int  => i.toLong
              case _       => -1L
            }
            if asLong != factVal then ok = false
        }
        i += 1
      }
      if ok then Iterator.single(current) else Iterator.empty
    }
  }

  private def evalPredicate(p: Q.Predicate, row: Map[String, Any]): Boolean = {
    val l = resolveTerm(p.lhs, row)
    val r = resolveTerm(p.rhs, row)
    (l, r) match {
      case (None, _) | (_, None) => false
      case (Some(a), Some(b))    => compareWith(p.op, a, b)
    }
  }

  private def resolveTerm(t: Q.Term, row: Map[String, Any]): Option[Any] = t match {
    case Q.Var(n) => row.get(n)
    case Q.C(c)   => Some(c)
  }

  private def compareWith(op: Q.Op, a: Any, b: Any): Boolean =
    op match {
      case Q.Op.Eq => a == b
      case Q.Op.Ne => a != b
      case _ =>
        val cmp = Compare.any(a, b)
        op match {
          case Q.Op.Lt => cmp < 0
          case Q.Op.Le => cmp <= 0
          case Q.Op.Gt => cmp > 0
          case Q.Op.Ge => cmp >= 0
          case _       => sys.error("unreachable")
        }
    }

  /** One-hop forward neighbours... every target eid stored under `attr` on `eid`.
    *
    * Returns empty when the eid has no outgoing `attr` datoms. Card-many fans out for free, since
    * each value lives as its own datom
    */
  private def forwardEdge(state: State, eid: Long, attr: String): Set[Long] =
    state.eavt.get(Eid(eid)).flatMap(_.get(attr))
      .map(_.iterator.map(_.asInstanceOf[Long]).toSet)
      .getOrElse(Set.empty)

  /** One-hop reverse neighbours via the AVET probe. */
  private def reverseEdge(state: State, eid: Long, attr: String): Set[Long] =
    state.lookup(attr, eid).map(Eid.value)

  /** BFS over `step`... every node reachable from `start` in 1+ hops.
    *
    * Cycles are guarded by the `seen` set; `start` itself is excluded from the result unless a real
    * cycle puts it back
    */
  private def closureBfs(start: Long, step: Long => Set[Long]): Set[Long] = {
    val out = scala.collection.mutable.Set.empty[Long]
    val seen = scala.collection.mutable.Set.empty[Long]
    seen += start
    var frontier: Set[Long] = step(start)
    while frontier.nonEmpty do {
      val next = scala.collection.mutable.Set.empty[Long]
      frontier.foreach { e =>
        if !seen(e) then {
          seen += e
          out += e
          step(e).foreach(n => if !seen(n) then next += n)
        }
      }
      frontier = next.toSet
    }
    out.toSet
  }

  private def matchClosure(
      state: State,
      cls: Q.Closure,
      row: Map[String, Any]
  ): Iterator[Map[String, Any]] = {
    def resolve(t: Q.Term): Option[Any] = t match {
      case Q.Var(n) => row.get(n)
      case Q.C(c)   => Some(c)
    }

    val srcKnown = resolve(cls.src)
    val dstKnown = resolve(cls.dst)

    def bind(t: Q.Term, v: Long, into: Map[String, Any]): Map[String, Any] = t match {
      case Q.Var(n) if !into.contains(n) => into.updated(n, v)
      case _                             => into
    }

    val fwd: Long => Set[Long] = forwardEdge(state, _, cls.attr)
    val rev: Long => Set[Long] = reverseEdge(state, _, cls.attr)

    (srcKnown, dstKnown) match {
      case (Some(sAny), Some(dAny)) =>
        val s = sAny.asInstanceOf[Long]
        val d = dAny.asInstanceOf[Long]
        if closureBfs(s, fwd).contains(d) then Iterator.single(row) else Iterator.empty
      case (Some(sAny), None) =>
        val s = sAny.asInstanceOf[Long]
        closureBfs(s, fwd).iterator.map(d => bind(cls.dst, d, row))
      case (None, Some(dAny)) =>
        val d = dAny.asInstanceOf[Long]
        closureBfs(d, rev).iterator.map(s => bind(cls.src, s, row))
      case (None, None) =>
        val sources: Set[Long] = state.eavt.iterator.collect {
          case (eid, attrs) if attrs.contains(cls.attr) => Eid.value(eid)
        }.toSet
        sources.iterator.flatMap { s =>
          closureBfs(s, fwd).iterator.map(d => bind(cls.dst, d, bind(cls.src, s, row)))
        }
    }
  }

  private def matchPattern(
      state: State,
      pat: Q.Pattern,
      row: Map[String, Any]
  ): Iterator[Map[String, Any]] = {
    def resolve(t: Q.Term): Option[Any] = t match {
      case Q.Var(n) => row.get(n)
      case Q.C(c)   => Some(c)
    }

    val knownE = resolve(pat.e)
    val knownV = resolve(pat.v)

    val candidates: Iterator[(Eid, Any)] = (knownE, knownV) match {
      case (Some(eAny), _) =>
        val eid = Eid(eAny.asInstanceOf[Long])
        state.eavt.get(eid).flatMap(_.get(pat.a))
          .iterator.flatMap(_.iterator).map(v => (eid, v))
      case (None, Some(vAny)) =>
        state.lookup(pat.a, vAny).iterator.map(eid => (eid, vAny))
      case (None, None) =>
        state.eavt.iterator.flatMap { case (eid, attrs) =>
          attrs.get(pat.a).iterator.flatMap(_.iterator).map(v => (eid, v))
        }
    }

    def bind(t: Q.Term, value: Any, into: Map[String, Any]): Map[String, Any] = t match {
      case Q.Var(n) if !into.contains(n) => into.updated(n, value)
      case _                             => into
    }

    candidates.flatMap { case (eid, v) =>
      if knownV.exists(_ != v) then Iterator.empty
      else Iterator.single(bind(pat.v, v, bind(pat.e, Eid.value(eid), row)))
    }
  }

  final class Live(storage: Storage) extends Db with DbInternal {
    private val ref = AtomicReference[State](State.rebuild(storage.load()))
    private val listeners = scala.collection.mutable.ListBuffer.empty[TxReport => Unit]

    def internal: DbInternal = this

    def all[A](using e: Entity[A]): Query[A] = {
      val s = ref.get
      Query(this, () => reconstructAll(s, e))
    }

    def byEqByName[A](attr: String, value: Any)(using e: Entity[A]): Query[A] =
      byEqQuery(this, ref.get, e, attr, value)

    def eidsByEq(attr: String, value: Any): Set[Long] =
      ref.get.lookup(attr, value).map(Eid.value)

    def eidsByEqByName[A](attr: String, value: Any)(using e: Entity[A]): Set[Long] =
      ref.get.lookup(s"${e.name}/$attr", value).map(Eid.value)

    def allEids[A](using e: Entity[A]): Set[Long] =
      ref.get.eidByClass.getOrElse(e.name, Set.empty).map(Eid.value)

    def eidOf[A](a: A)(using e: Entity[A]): Option[Long] =
      findEidFor(ref.get, widen(e), a).map(Eid.value)

    def pullCtx: PullCtx = new StateCtx(ref.get)

    def pullManyEids[A, R](eids: Iterable[Long])(f: Pull[A] => R)(using e: Entity[A]): Vector[R] =
      pullEids(ref.get, eids, e, f)

    def evalClauses(clauses: Vector[Q.Clause]): Iterator[Map[String, Any]] =
      evalClausesAt(ref.get, clauses)

    def liveDatoms: Vector[Datom] = liveDatomsOf(ref.get)

    def byEid[A](eid: Long)(using e: Entity[A]): Option[A] =
      eidLookup(ref.get, eid, e)

    def transact(tx: Tx): TxReport = {
      val now = Time.now()
      @scala.annotation.tailrec
      def cas(): (TxReport, Vector[Datom]) = {
        val cur = ref.get
        val (next, rep) = cur.commit(tx, now)
        if ref.compareAndSet(cur, next) then (rep, next.log.drop(cur.log.size))
        else cas()
      }
      val (report, newDatoms) = cas()
      if newDatoms.nonEmpty then storage.append(newDatoms)
      listeners.synchronized(listeners.foreach(_(report)))
      report
    }

    def withTx(tx: Tx): Db = {
      val (next, _) = ref.get.commit(tx, Time.now())
      new Snap(next)
    }

    def snapshot: Db = new Snap(ref.get)

    def asOf(tx: TxId): Db = {
      val s = ref.get
      val slice = s.log.takeWhile(_.t.value <= tx.value)
      new Snap(State.rebuild(slice))
    }

    @scala.annotation.targetName("asOfTime")
    def asOf(at: Time): Db = {
      val s = ref.get
      val slice = s.log.takeWhile(_.time <= at)
      new Snap(State.rebuild(slice))
    }

    def listen(f: TxReport => Unit): Unit =
      listeners.synchronized { listeners += f; () }

    def log: Vector[Datom] = ref.get.log

    def historyOf[A](a: A)(using e: Entity[A]): Vector[Datom] =
      historyForValue(ref.get, e, a)

    def history(eid: Long): Vector[Datom] =
      ref.get.log.filter(_.e == Eid(eid))

    def pretty: String = prettyOf(ref.get)
    def prettyLog(color: Boolean = false): String = prettyLogOf(ref.get.log, color)
  }

  final class Snap(state: State) extends Db with DbInternal {
    def internal: DbInternal = this

    def all[A](using e: Entity[A]): Query[A] =
      Query(this, () => reconstructAll(state, e))

    def byEqByName[A](attr: String, value: Any)(using e: Entity[A]): Query[A] =
      byEqQuery(this, state, e, attr, value)

    def eidsByEq(attr: String, value: Any): Set[Long] =
      state.lookup(attr, value).map(Eid.value)

    def eidsByEqByName[A](attr: String, value: Any)(using e: Entity[A]): Set[Long] =
      state.lookup(s"${e.name}/$attr", value).map(Eid.value)

    def allEids[A](using e: Entity[A]): Set[Long] =
      state.eidByClass.getOrElse(e.name, Set.empty).map(Eid.value)

    def eidOf[A](a: A)(using e: Entity[A]): Option[Long] =
      findEidFor(state, widen(e), a).map(Eid.value)

    def pullCtx: PullCtx = new StateCtx(state)

    def pullManyEids[A, R](eids: Iterable[Long])(f: Pull[A] => R)(using e: Entity[A]): Vector[R] =
      pullEids(state, eids, e, f)

    def evalClauses(clauses: Vector[Q.Clause]): Iterator[Map[String, Any]] =
      evalClausesAt(state, clauses)

    def liveDatoms: Vector[Datom] = liveDatomsOf(state)

    def byEid[A](eid: Long)(using e: Entity[A]): Option[A] =
      eidLookup(state, eid, e)

    def transact(tx: Tx): TxReport =
      throw UnsupportedOperationException("snapshot is read-only; transact on a live Db")

    def withTx(tx: Tx): Db = {
      val (next, _) = state.commit(tx, Time.now())
      new Snap(next)
    }

    def snapshot: Db = this

    def asOf(tx: TxId): Db = {
      val slice = state.log.takeWhile(_.t.value <= tx.value)
      new Snap(State.rebuild(slice))
    }

    @scala.annotation.targetName("asOfTime")
    def asOf(at: Time): Db = {
      val slice = state.log.takeWhile(_.time <= at)
      new Snap(State.rebuild(slice))
    }

    def listen(f: TxReport => Unit): Unit = ()

    def log: Vector[Datom] = state.log

    def historyOf[A](a: A)(using e: Entity[A]): Vector[Datom] =
      historyForValue(state, e, a)

    def history(eid: Long): Vector[Datom] =
      state.log.filter(_.e == Eid(eid))

    def pretty: String = prettyOf(state)
    def prettyLog(color: Boolean = false): String = prettyLogOf(state.log, color)
  }

  /** Pinned read-only view of one `State`.
    *
    * Lookups inside a single pull closure all see the same world because the state is captured once
    * at pull-call time
    */
  final private class StateCtx(state: State) extends PullCtx {
    def scalarOf(eid: Long, attr: String): Any =
      state.eavt.get(Eid(eid)).flatMap(_.get(attr)).map(_.head).getOrElse(
        throw new RuntimeException(s"pull: no value at eid=$eid attr=$attr")
      )

    def multiValueOf(eid: Long, attr: String): Set[Any] =
      state.eavt.get(Eid(eid)).flatMap(_.get(attr)).getOrElse(Set.empty)

    def refEidOf(eid: Long, attr: String): Long =
      state.eavt.get(Eid(eid)).flatMap(_.get(attr)).map(_.head.asInstanceOf[Long]).getOrElse(
        throw new RuntimeException(s"pull: no ref at eid=$eid attr=$attr")
      )

    def multiRefEidsOf(eid: Long, attr: String): Vector[Long] =
      state.eavt.get(Eid(eid)).flatMap(_.get(attr))
        .map(_.toVector.map(_.asInstanceOf[Long])).getOrElse(Vector.empty)

    def reconstructAt[A](eid: Long, e: Entity[A]): A =
      reconstructEntity(state, Eid(eid), e)

    def existsAs[A](eid: Long, e: Entity[A]): Boolean =
      state.eidByClass.getOrElse(e.name, Set.empty).contains(Eid(eid))

    def reverseEids(attr: String, eid: Long): Vector[Long] =
      state.lookup(attr, eid).iterator.map(Eid.value).toVector
  }

  private def pullEids[A, R](
      state: State,
      eids: Iterable[Long],
      e: Entity[A],
      f: Pull[A] => R
  ): Vector[R] = {
    val ctx = new StateCtx(state)
    eids.iterator.map(eid => f(new Pull[A](ctx, eid, e))).toVector
  }

  /** Walk the log once...
    *
    * Keeps the most recent `Op.Assert` per `(eid, attr, value)` triple and drops triples whose
    * latest entry is a `Op.Retract`. Insertion order is preserved so diffs stay readable in tx
    * order
    */
  private def liveDatomsOf(state: State): Vector[Datom] = {
    val live = scala.collection.mutable.LinkedHashMap.empty[(Eid, String, Any), Datom]
    state.log.foreach { d =>
      val k = (d.e, d.a, d.v)
      d.op match {
        case Op.Assert  => live(k) = d
        case Op.Retract => live.remove(k); ()
      }
    }
    live.values.toVector
  }

  /** Filter the log to datoms touching the entity that currently matches `value` by structural key.
    *
    * Returns asserts and retracts in tx order. Empty if no eid currently corresponds to `value`
    */
  private def historyForValue[A](state: State, e: Entity[A], value: A): Vector[Datom] =
    findEidFor(state, widen(e), value) match {
      case None      => Vector.empty
      case Some(eid) => state.log.filter(_.e == eid)
    }

  /** Render any datom value for the pretty printers.
    *
    * Strings get quoted, everything else uses its own `toString`. The optional `state` lets ref
    * values (a `Long` that points to a known eid) render as `#n` so the eye can follow them back to
    * the entity block
    */
  private def renderValue(v: Any, state: State = null): String = v match {
    case s: String                                               => "\"" + s + "\""
    case null                                                    => "null"
    case n: Long if state != null && state.eavt.contains(Eid(n)) => s"#$n"
    case other                                                   => other.toString
  }

  private def prettyOf(state: State): String = {
    val classes = state.eidByClass.toVector.filter(_._2.nonEmpty).sortBy(_._1)
    if classes.isEmpty then "(empty db)\n"
    else {
      val sb = new StringBuilder
      classes.foreach { case (cls, eids) =>
        val sortedEids = eids.toVector.sortBy(Eid.value)
        sb.append(s"$cls (${eids.size})\n")
        val prefix = s"$cls/"
        sortedEids.foreach { eid =>
          val attrs = state.eavt.getOrElse(eid, Map.empty).toVector.sortBy(_._1)
          val rows = attrs.map { case (a, vs) =>
            val name = if a.startsWith(prefix) then a.drop(prefix.length) else a
            val rendered =
              if vs.size == 1 then renderValue(vs.head, state)
              else vs.toVector.map(renderValue(_, state)).sorted.mkString("{", ", ", "}")
            name -> rendered
          }
          val w = rows.map(_._1.length).maxOption.getOrElse(0)
          sb.append(s"  #${Eid.value(eid)}\n")
          rows.foreach { case (n, v) =>
            sb.append(s"    ${n.padTo(w, ' ')} = $v\n")
          }
        }
        sb.append("\n")
      }
      sb.result()
    }
  }

  private def prettyLogOf(log: Vector[Datom], color: Boolean): String =
    if log.isEmpty then "(empty log)\n"
    else {
      val headers = Vector("tx", "time", "eid", "attr", "value", "op")
      val rows = log.map { d =>
        val cells = Vector(
          d.t.value.toString,
          d.time.epochMillis.toString,
          "#" + Eid.value(d.e).toString,
          d.a,
          renderValue(d.v),
          if d.op == Op.Assert then "+" else "-"
        )
        cells -> d.op
      }
      val widths = headers.indices.map { i =>
        (headers(i) +: rows.map(_._1(i))).map(_.length).max
      }.toVector
      val sb = new StringBuilder
      def writeRow(r: Vector[String], op: Option[Op]): Unit = {
        val tinted = op.filter(_ => color).map {
          case Op.Assert  => "[32m"
          case Op.Retract => "[31m"
        }
        tinted.foreach(sb.append)
        r.iterator.zipWithIndex.foreach { case (s, i) =>
          sb.append(s.padTo(widths(i), ' '))
          if i < r.size - 1 then sb.append("  ")
        }
        if tinted.nonEmpty then sb.append("[0m")
        sb.append("\n")
      }
      writeRow(headers, None)
      writeRow(widths.map(w => "-" * w), None)
      rows.foreach { case (r, op) => writeRow(r, Some(op)) }
      sb.result()
    }
}
