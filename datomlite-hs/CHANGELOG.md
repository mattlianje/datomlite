# Changelog

## 0.1.0.0 (unreleased)

Initial release.

- `Db` is an immutable value: an append-only datom log with EAVT and AVET indexes. Start from `emptyDb`
- Records are the schema. Derive `Entity` and the field types say the rest: `Key a`, `Maybe a`, `Set a`, `Ref t`
- Fields hold `Text`, `Int`, `Integer`, `Double`, `Bool`, `ByteString`, `UTCTime`, `Day` or `Ref t`. Enums and newtypes derive their instances
- Bad schemas are type errors with a readable message (a `String` field, a list field, a sum type, a misspelled field name)
- `Tx` monad with `new`, `save`, `update`, `setField`, `retractEntity` and `abort`. `commit` is pure and atomic and checks `Key` uniqueness
- `load` and `loadAll`. Facts that don't decode throw a `LoadError` instead of disappearing
- Queries are do-blocks: `match`, `matchBy` (through AVET), `follow`, `oneOf`, `exists` / `none`
- Time travel with `asOf`, `since`, `history` and `datoms`
- `fromHistory` rebuilds a `Db` from its datoms. `show` / `read` round-trips a datom per line
- `Conn` and `commitIO` for the IO side
- `listen` and `watch`. Each subscriber gets its own thread so writers never wait on it
- `Datomlite.Raw` for going around the schema
