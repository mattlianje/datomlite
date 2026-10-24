package datomlite

/** Pluggable backing store for a `Db`
  */
trait Storage {
  def load(): Vector[Datom]
  def append(datoms: Vector[Datom]): Unit
  def close(): Unit = ()
}

object Storage {
  val none: Storage = new Storage {
    def load(): Vector[Datom] = Vector.empty
    def append(datoms: Vector[Datom]): Unit = ()
  }
}
