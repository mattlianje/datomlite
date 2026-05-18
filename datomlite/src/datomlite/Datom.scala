package datomlite

opaque type Eid = Long
object Eid {
  def apply(n: Long): Eid = n
  extension (e: Eid) def value: Long = e
}

final case class Datom(e: Eid, a: String, v: Any, t: TxId, time: Time, op: Op)
