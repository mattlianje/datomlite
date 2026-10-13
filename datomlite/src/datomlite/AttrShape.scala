package datomlite

/** Per-field storage shape detected at compile time by `Entity.derived`
  *
  * Decides how a value splits into datoms and whether it's a ref to another entity. Value shapes
  * carry the `Codec` for the field's stored form.. ref shapes just store the target eid
  */
sealed trait FieldShape {
  def codec: Codec[Any]
}

object FieldShape {
  final case class Value(codec: Codec[Any]) extends FieldShape

  /** `Set[T]` of a non-ref `T`. One datom per element, each through `codec` */
  final case class MultiValue(codec: Codec[Any]) extends FieldShape

  /** `Option[T]` of a non-ref `T`
    *
    * `None` writes no datom; `Some(v)` writes one datom whose value is `v` (the inner). Reconstruct
    * produces `None` when the attribute has no datoms, `Some(head)` otherwise. Indexes ride the
    * AVET like a plain value
    */
  final case class Optional(codec: Codec[Any]) extends FieldShape

  /** Self-reference safe ref target
    *
    * `target` is by-name so an entity's `Entity[A]` can reference itself (e.g. `case class N(id:
    * Int, next: Set[N])`)
    */
  final case class Ref(target: () => Entity[?]) extends FieldShape {
    def codec: Codec[Any] = Codec.eid
  }

  final case class MultiRef(target: () => Entity[?]) extends FieldShape {
    def codec: Codec[Any] = Codec.eid
  }

  /** `Option[R]` of a ref `R`. `None` writes no datom, same as `Optional` */
  final case class OptionalRef(target: () => Entity[?]) extends FieldShape {
    def codec: Codec[Any] = Codec.eid
  }
}
