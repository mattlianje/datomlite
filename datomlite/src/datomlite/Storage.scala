package datomlite

/** Pluggable backing store for a `Conn`. Read one into a plain value with `Db.load`
  */
trait Storage {
  def load(): Vector[Datom]

  /** Called once per tx in tx order, under the conn's write lock and before anyone can see the
    * commit. Throwing aborts the tx
    */
  def append(datoms: Vector[Datom]): Unit
  def close(): Unit = ()
}

object Storage {
  val none: Storage = new Storage {
    def load(): Vector[Datom] = Vector.empty
    def append(datoms: Vector[Datom]): Unit = ()
  }
}
