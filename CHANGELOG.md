# Changelog

## Unreleased

- `Codec[T]` for value types. `derives Codec` on case classes and enums, `Codec.of` for anything else
- `Date` (a calendar day) next to `Time`
- `Option[R]` fields where `R` is an entity are optional refs
- Old data still reads after you add a field with a default, drop one, or widen `Int` to `Long`
- `Migration` for renames and removals, run on load with `Storage.migrate`
- `Tx.fn(db => tx)` reads and writes in one commit. `db.tx`, `retractWhere` and `upsertWhere` use it so concurrent updates don't get lost
- Writes are serialized, reads never block. A failing `Storage.append` aborts the tx
- `TxReport.datoms`
- Listeners hear each commit once and in order. One that throws doesn't break the tx or the others
- `>`, `>=`, `<`, `<=` and `startsWith` use the index instead of scanning
- `db.watch(q)(f)` for live queries
- **Breaking:** `listen` returns a `Subscription`
- **Breaking:** transacting on the outer db inside `db.tx` or `Tx.fn` throws. Use `b` instead

## 0.1.0 — 2026-05-18

Initial release.

- Core datomlite DB impl + Query builder impl
- Datalog joins, refs, aggregations, window functions, recursive rules
- Time-travel, history, diff, speculative transactions
- Negation and existential subgoals
- Pluggable `Storage` backend
