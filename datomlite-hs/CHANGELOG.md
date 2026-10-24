# Changelog

## 0.1.0.0 (unreleased)

Initial release.

- `Db` is an immutable value: an append-only datom log with EAVT and AVET indexes. Start from `emptyDb`
- Records are the schema. Derive `Entity` and the field types say the rest: `Maybe a`, `Set a`, `Ref t`
- `type Identity T = '["email", ...]` names the unique fields, each its own identity. The record stays plain and a misspelled name is a type error
- Fields hold `Text`, `Int`, `Integer`, `Double`, `Bool`, `ByteString`, `UTCTime`, `Day` or `Ref t`. Enums and newtypes derive their instances
- Bad schemas are type errors with a readable message (a `String` field, a list field, a sum type, a misspelled field or identity name)
- Writes are a list: `commit_ db [Insert r, Upsert r, Update ref f, Delete ref]`, in order, all or nothing. `commit` is pure and checks `Identity` uniqueness
- `Tx` do-blocks for steps that need each other: `insert`, `upsert`, `update`, `setField`, `retract`, `txDb` and `abort`. `commit` takes a list or a `Tx`
- `UniqueViolation` names the entities that would share the value. `UnknownIdentity` for a lookup ref nothing holds
- Every read hands back a `Stored a`: fields by `p.name`, the ref by `refOf`, the record by `recordOf`. `load`, `every`, and `find` / `findBy` by identity. Facts that don't decode throw a `LoadError` instead of disappearing
- Lookup refs: `ref @Department "ENG"` goes anywhere a `Ref` does and commit swaps in the entity, even one written earlier in the same `Tx`
- Queries are do-blocks: `match`, `matchBy` (through AVET), `follow`, `oneOf`, `exists` / `none`
- `closure` for recursive queries over graphs with cycles
- Window functions over results: `window` with `rowNumber`, `rank`, `denseRank`, `lag`, `lead`, `whole`, `running`
- Time travel with `asOf`, `since`, `history` and `datoms`
- `transact` stamps each tx with the time. Rewind to one with `asOfTime`, or stamp your own with `commitAt`
- `fromHistory` rebuilds a `Db` from its datoms. `show` / `read` round-trips a datom per line
- `Conn` for the IO side. `transact` is `commit` on a `Conn` and hands back the same result and new `Db`. `snapshot` reads what it holds now
- `listen` and `watch`. Each subscriber gets its own thread so writers never wait on it
- `Datomlite.Raw` for going around the schema
