package datomlite

import java.util.concurrent.ConcurrentLinkedQueue
import scala.jdk.CollectionConverters.*

class ConcurrencyTest extends munit.FunSuite:

  case class Counter(name: String, n: Long)
  object Counter:
    given Entity[Counter] = Entity.derived[Counter].key(_.name)

  final class MemStorage extends Storage:
    private var datoms = Vector.empty[Datom]
    def load(): Vector[Datom] = synchronized(datoms)
    def append(ds: Vector[Datom]): Unit = synchronized(datoms ++= ds)

  private def inParallel(threads: Int)(body: Int => Unit): Unit =
    val ts = (0 until threads).map(i => new Thread(() => body(i)))
    ts.foreach(_.start())
    ts.foreach(_.join())

  test("concurrent upsertWhere increments never lose an update"):
    val db = Db(Counter("hits", 0L))
    inParallel(8)(_ =>
      (1 to 250).foreach(_ => db.upsertWhere[Counter](_.name == "hits")(c => c.copy(n = c.n + 1L)))
    )
    assertEquals(db.where[Counter].one.map(_.n), Some(2000L))

  test("concurrent tx blocks read-modify-write atomically"):
    val db = Db(Counter("a", 0L), Counter("b", 0L))
    inParallel(8) { _ =>
      (1 to 100).foreach { _ =>
        db.tx { b =>
          val a = b.db.where[Counter](_.name == "a").one.get
          b.upsert(a.copy(n = a.n + 1L), Counter("b", a.n + 1L))
        }
      }
    }
    assertEquals(db.where[Counter].run.map(_.n).toSet, Set(800L))

  test("storage receives datoms in tx order, matching the in-memory log"):
    val storage = MemStorage()
    val db = Db(storage)
    inParallel(8)(t => (1 to 50).foreach(i => db.add(Counter(s"t$t-$i", i.toLong))))
    assertEquals(storage.load(), db.log)
    assertEquals(Db(storage).where[Counter].run.toSet, db.where[Counter].run.toSet)

  test("listeners see every commit once, in tx order, under concurrent writers"):
    val db = Db()
    val seen = new ConcurrentLinkedQueue[Long]()
    db.listen(rep => seen.add(rep.tx.value))
    inParallel(8)(t => (1 to 50).foreach(i => db.add(Counter(s"t$t-$i", 0L))))
    val txs = seen.asScala.toVector
    assertEquals(txs.size, 400)
    assertEquals(txs, txs.sorted)
    assertEquals(txs.distinct.size, 400)

  test("watch settles on the final result under concurrent writers"):
    val db = Db()
    @volatile var last = -1L
    db.watch(_.count[Counter])(n => last = n)
    inParallel(4)(t => (1 to 50).foreach(i => db.add(Counter(s"t$t-$i", 0L))))
    assertEquals(last, 200L)
