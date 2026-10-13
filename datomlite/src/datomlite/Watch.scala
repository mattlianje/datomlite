package datomlite

import scala.annotation.implicitNotFound
import scala.concurrent.ExecutionContext
import scala.util.NotGiven
import scala.util.control.NonFatal

/** What `db.listen` and `db.watch` hand back
  *
  * `cancel()` stops the callbacks. Safe to call twice. `close()` does the same so it works with
  * `Using`
  */
final class Subscription private[datomlite] (onCancel: () => Unit) extends AutoCloseable {
  @volatile private var live = true
  def isActive: Boolean = live
  def cancel(): Unit = if live then { live = false; onCancel() }
  def close(): Unit = cancel()
}

object Subscription {

  /** Already cancelled. Read-only views hand this back since they never fire */
  private[datomlite] def inactive: Subscription = {
    val s = new Subscription(() => ())
    s.cancel()
    s
  }
}

/** Proof a watched result is a plain value we can compare with `==` between commits
  *
  * `Query` and `TypedSelector` are lazy and never compare equal
  */
@implicitNotFound(
  "db.watch needs a materialized result to compare between commits; end the query with .run, " +
    ".one, .count or .toSet, got ${T}"
)
sealed trait Eager[T]

object Eager {
  private object instance extends Eager[Any]
  given [T](using NotGiven[T <:< Query[?]], NotGiven[T <:< TypedSelector[?]]): Eager[T] =
    instance.asInstanceOf[Eager[T]]
}

/** Which entity classes a read touched
  *
  * Read paths report into one of these. The base class ignores it all... `Deps.Recorder` keeps the
  * classes so a watch only re-runs when a commit writes one of them
  */
private[datomlite] class Deps {
  def entity(e: Entity[?]): Unit = ()
  def attr(a: String): Unit = ()
  def clauses(cs: Vector[Q.Clause]): Unit = ()
  def everything(): Unit = ()
}

private[datomlite] object Deps {
  val off: Deps = new Deps

  /** `"Employee/name"` -> `"Employee"` */
  def classOf(attr: String): String = attr.takeWhile(_ != '/')

  final class Recorder extends Deps {
    private var all = false
    private val classes = scala.collection.mutable.Set.empty[String]
    private val rules = scala.collection.mutable.Set.empty[AnyRef]

    /** Loading an entity walks its refs so their targets count as read too */
    override def entity(e: Entity[?]): Unit =
      if classes.add(e.name) then
        e.shapes.foreach {
          case FieldShape.Ref(t)         => entity(t())
          case FieldShape.MultiRef(t)    => entity(t())
          case FieldShape.OptionalRef(t) => entity(t())
          case _                         => ()
        }

    override def attr(a: String): Unit = { classes += classOf(a); () }

    override def clauses(cs: Vector[Q.Clause]): Unit = cs.foreach {
      case p: Q.Pattern => attr(p.a)
      case c: Q.Closure => attr(c.attr)
      case r: Q.RuleCall if rules.add(r.rule) =>
        r.rule.asInstanceOf[Rule[?, ?]].bodies.foreach(b => clauses(b.clauses))
      case _ => ()
    }

    override def everything(): Unit = all = true

    /** Nothing recorded means the closure read some other `Db`... so re-run on everything */
    def touches(written: Set[String]): Boolean =
      all || classes.isEmpty || classes.exists(written)
  }
}

/** Hands commits to listeners and watches in tx order
  *
  * Commits enqueue under the db's write lock so the queue is in tx order. The first caller of
  * `drain` delivers everything queued, one event at a time. Anyone else who calls it meanwhile (a
  * listener that writes, say) sees the drain running and leaves their event to it. A callback that
  * throws gets reported and never reaches the committer
  */
final private[datomlite] class Hub {
  import DbImpl.State

  abstract private class Sink {
    @volatile var active = true

    /** Skip events queued before we subscribed */
    var since = 0L
    def onTx(rep: TxReport, written: => Set[String], state: State): Unit
  }

  final private class ListenSink(f: TxReport => Unit) extends Sink {
    def onTx(rep: TxReport, written: => Set[String], state: State): Unit = f(rep)
  }

  final private class WatchSink[T](q: Db => T, f: T => Unit) extends Sink {
    private var deps: Option[Deps.Recorder] = None
    private var last: Option[T] = None

    def run(state: State): Unit = {
      val rec = new Deps.Recorder
      deps = None
      val v = q(new DbImpl.Snap(state, rec))
      deps = Some(rec)
      if !last.contains(v) then {
        last = Some(v)
        f(v)
      }
    }

    def onTx(rep: TxReport, written: => Set[String], state: State): Unit =
      if deps.forall(_.touches(written)) then run(state)
  }

  private enum Event {
    case Committed(seq: Long, rep: TxReport, state: State)
    case Init(sink: WatchSink[?], state: State)
  }

  private val lock = new Object
  private var sinks = Vector.empty[Sink]
  private val pending = scala.collection.mutable.Queue.empty[Event]
  private var seq = 0L
  private var draining = false

  def listen(f: TxReport => Unit): Subscription = subscribe(new ListenSink(f), None)

  /** Add a watch whose first run sees `state`. Call under the db's write lock so no commit slips in
    * between
    */
  def watch[T](q: Db => T, f: T => Unit, state: State): Subscription = {
    val s = new WatchSink(q, f)
    subscribe(s, Some(Event.Init(s, state)))
  }

  private def subscribe(s: Sink, init: Option[Event]): Subscription = {
    lock.synchronized {
      s.since = seq
      sinks = sinks :+ s
      init.foreach(pending.enqueue)
    }
    new Subscription(() => {
      s.active = false
      lock.synchronized { sinks = sinks.filterNot(_ eq s) }
    })
  }

  /** Queue a commit. Call under the db's write lock so events queue in tx order */
  def committed(rep: TxReport, state: State): Unit = lock.synchronized {
    if sinks.nonEmpty then {
      seq += 1
      pending.enqueue(Event.Committed(seq, rep, state))
    }
  }

  /** Deliver what's queued, unless someone else already is */
  def drain(): Unit = {
    val mine = lock.synchronized {
      val go = !draining && pending.nonEmpty
      if go then draining = true
      go
    }
    if mine then {
      var done = false
      try {
        var ev = next()
        while ev.isDefined do {
          deliver(ev.get)
          ev = next()
        }
        done = true
      } finally if !done then lock.synchronized { draining = false }
    }
  }

  /** Next event. When there's none, give up the drain under the same lock */
  private def next(): Option[Event] = lock.synchronized {
    if pending.isEmpty then {
      draining = false
      None
    } else Some(pending.dequeue())
  }

  private def deliver(ev: Event): Unit = ev match {
    case Event.Committed(n, rep, state) =>
      lazy val written = rep.datoms.iterator.map(d => Deps.classOf(d.a)).toSet
      lock.synchronized(sinks).foreach { s =>
        if s.active && n > s.since then guard(s.onTx(rep, written, state))
      }
    case Event.Init(s, state) =>
      if s.active then guard(s.run(state))
  }

  private def guard(body: => Unit): Unit =
    try body
    catch { case NonFatal(t) => ExecutionContext.defaultReporter(t) }
}
