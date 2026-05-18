# notes

Working notes on direction. Not user-facing docs.

## Accretional schema

Datomlite is a store for case classes that evolve purely accretionally and type-preservingly. A case class can grow, never shrink, never rename, never change a field's type.

New fields are allowed in one of these shapes:

- `Option[T]`. Old eids reconstruct as `None`.
- `Set[T]`. Old eids reconstruct as `Set.empty`.
- ref to a keyed entity. Old eids reconstruct as the absence-of-ref case (TBD: `Option[Ref]` is currently rejected at derivation, see `Entity.scala:54`).
- required `T` with a declared `.default(_.field, value)` on the `Entity`.

A rename, removal, or type change is not an evolution, it is a new entity. Make `PersonV2` and migrate at the application layer if needed. The absence of a rename/migrate API is the feature.

## Defaults are first-class

`.default` is not just an evolution-time afterthought. Any required field on an `Entity` may carry one. The macro just asks: at reconstruct time, if the attr is missing for this eid, what value goes in? Answer is either "the declared default" or "throw."

That makes the rule symmetric. New fields with defaults are accretional. Existing fields with defaults are robust to historical retracts. Same primitive.

## Enforcement

Two layers:

1. Source-time. `Entity.derived[A]` validates the case class shape (no `Option[Ref]` etc.). It cannot see history.
2. Boot-time, only against persistent storage (`FileStorage`, BYO). After `Storage.load()`, walk the distinct `(entityName, attr)` pairs in the log. For each one, check the live `Entity[*]` registry:
   - attr maps to a current field (no removal, no rename)
   - the historical type tag matches the current field's storage type (no type change)
   - if the field is required and was absent in some historical tx, a `.default` is declared
   Any violation throws `SchemaDrift(...)` with a precise message at boot, not at first read.

In-memory dbs (`Storage.none`) skip the check. The log does not outlive the code, so there is no drift to catch.

## Proposed API

Day 1. Same as today.

```scala
case class Person(email: String, name: String)
given Entity[Person] = Entity.derived[Person].key(_.email)

val db = Db(FileStorage(Path.of("db.dlite")))
db.add(Person("m@x.io", "Matt"))
```

Six months later. Add an optional or set field. Free.

```scala
case class Person(email: String, name: String, nickname: Option[String], tags: Set[String])
given Entity[Person] = Entity.derived[Person].key(_.email)

val db = Db(FileStorage(Path.of("db.dlite")))
// boots fine. Matt reconstructs as Person("m@x.io", "Matt", None, Set.empty).
```

Add a required field. Needs a default.

```scala
case class Person(email: String, name: String, salary: Long)
given Entity[Person] = Entity.derived[Person]
  .key(_.email)
  .default(_.salary, 0L)

val db = Db(FileStorage(Path.of("db.dlite")))
// boots. Old Matt reconstructs as Person("m@x.io", "Matt", 0L).
```

Without the default, boot refuses.

```
SchemaDrift: Person/salary is required on Entity[Person] but 2 historical
  eids have no Person/salary datom. Declare .default(_.salary, ...) or
  change the field to Option[Long].
```

Rename or change a type. Not allowed. Make a new entity.

```scala
// Person/name lives in the log forever. You want fullName. Don't rename.
case class PersonV2(email: String, fullName: String)
given Entity[PersonV2] = Entity.derived[PersonV2].key(_.email)

// move data at the application layer if you want
db.where[Person].run.foreach(p => db.add(PersonV2(p.email, p.name)))
// Person and PersonV2 coexist. asOf works on both.
```

A rename in source against an unchanged class name is caught at boot.

```
SchemaDrift: Person/name is in the log but not a field of Entity[Person].
  Renaming a field is not supported. Either restore the field, or
  define a new entity (e.g. PersonV2) and migrate.
```

A type change too.

```
SchemaDrift: Person/age was stored as Int (tag `i`), current
  Entity[Person].age is Long. Type changes are not supported.
  Define a new entity and migrate.
```

Introspection.

```scala
db.schema
// Vector(
//   AttrInfo("Person", "email",  Value, firstTx = 1, lastTx = 42),
//   AttrInfo("Person", "name",   Value, firstTx = 1, lastTx = 42),
//   AttrInfo("Person", "salary", Value, firstTx = 7, lastTx = 42),
// )

db.schema.of[Person]      // filtered to one entity
db.schema.entities        // Vector("Person", "Order", ...)
```

Under the accretional contract, `db.schema` and the union of in-scope `Entity[*]` describe the same set. The boot check is exactly what enforces that.

The whole new surface, in one block.

```scala
trait Entity[A]:
  def default[T](f: A => T, value: T): Entity[A]   // new

case class AttrInfo(
    entityName: String,
    fieldName:  String,
    shape:      FieldShape,
    firstTx:    TxId,
    lastTx:     TxId
)

extension (db: Db)
  def schema: Vector[AttrInfo]

class SchemaDrift(message: String) extends RuntimeException(message)
```

Two new methods, one exception, one info case class. Everything else (the boot check, the rejection of renames and type changes) is internal to `Db` and `Storage.load`.

## Open questions

- Entity registry. The boot-time check needs to know every entity the user intends to use. Currently `Entity[A]` is summoned at the call site only. Likely shape: a separate `entities*` arg to `Db(storage, ...)`, or a `given Registry` collected from in-scope `Entity[*]` instances.
- `Option[Ref]`. Today rejected. Needed for the "new ref field" accretional case. Either lift the restriction or pick a sentinel.
- `db.schema` introspection. `Vector[AttrInfo]` with `(entityName, fieldName, fieldShape, firstTx, lastTx)`. Useful for debugging drift errors and for the README's "your schema, materialized" story.
