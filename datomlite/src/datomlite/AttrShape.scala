package datomlite

/** Per-field storage shape detected at compile time by `Entity.derived`
  *
  * Decides how a value is split into datoms, and whether it is a ref to another entity
  */
sealed trait FieldShape

object FieldShape {
  case object Value extends FieldShape
  case object MultiValue extends FieldShape

  /** `Option[T]` of a non-ref `T`
    *
    * `None` writes no datom; `Some(v)` writes one datom whose value is `v` (the inner). Reconstruct
    * produces `None` when the attribute has no datoms, `Some(head)` otherwise. Indexes ride the
    * AVET like a plain value
    */
  case object Optional extends FieldShape

  /** Self-reference safe ref target
    *
    * `target` is by-name so an entity's `Entity[A]` can reference itself (e.g. `case class N(id:
    * Int, next: Set[N])`)
    */
  final case class Ref(target: () => Entity[?]) extends FieldShape
  final case class MultiRef(target: () => Entity[?]) extends FieldShape
}
