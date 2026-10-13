package datomlite

/** Pluggable backing store for a `Db`
  */
trait Storage {
  def load(): Vector[Datom]

  /** Called once per tx in tx order, under the write lock and before anyone can see the commit.
    * Throwing aborts the tx
    */
  def append(datoms: Vector[Datom]): Unit
  def close(): Unit = ()

  /** Same storage with `migrations` run in order over every datom it loads. Writes go through as is
    */
  def migrate(migrations: Migration*): Storage = {
    val self = this
    val all = migrations.reduceOption(_.andThen(_))
    new Storage {
      def load(): Vector[Datom] = all.fold(self.load())(m => self.load().flatMap(m(_)))
      def append(datoms: Vector[Datom]): Unit = self.append(datoms)
      override def close(): Unit = self.close()
    }
  }
}

object Storage {
  val none: Storage = new Storage {
    def load(): Vector[Datom] = Vector.empty
    def append(datoms: Vector[Datom]): Unit = ()
  }
}
