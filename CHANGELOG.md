# Changelog

## Unreleased

- **Breaking:** `Db` is an immutable value. `add`, `retract`, `upsert`, `retractWhere`, `upsertWhere` and `tx` return the new `Db`; `transact` returns a `TxReport`
- **Breaking:** `Conn` holds a changing db. `Conn()`, `Conn(db)` or `Conn(storage)`; `conn.db` is the current value. Same write verbs, returning `TxReport`
- **Breaking:** `listen` and `watch` live on `Conn`. `snapshot` and `withTx` are gone (a `Db` already is a snapshot, and any write is a branch)
- **Breaking:** `Db(storage, values*)` is now `Conn(storage)`. `Db.load(storage)` reads a storage into a plain value
- `TxReport.dbBefore` / `TxReport.dbAfter`
- `Tx.block`, `Tx.retractWhere`, `Tx.upsertWhere` build the same txs as values
- `Db.empty`
- `Codec[T]` for value types. `derives Codec` on case classes and enums, `Codec.of` for anything else
- `Date` (a calendar day) next to `Time`
- `Option[R]` fields where `R` is an entity are optional refs
- `Tx.fn(db => tx)` reads and writes in one commit. `db.tx`, `retractWhere` and `upsertWhere` use it so concurrent updates don't get lost
- Writes are serialized, reads never block. A failing `Storage.append` aborts the tx
- `TxReport.datoms`
- Listeners hear each commit once and in order. One that throws doesn't break the tx or the others
- `>`, `>=`, `<`, `<=` and `startsWith` use the index instead of scanning
- `conn.watch(q)(f)` for live queries
- **Breaking:** `listen` returns a `Subscription`
- **Breaking:** transacting on the conn inside `conn.tx` or `Tx.fn` throws. Use `b` instead

## 0.1.0 — 2026-05-18

Initial release.

- Core datomlite DB impl + Query builder impl
- Datalog joins, refs, aggregations, window functions, recursive rules
- Time-travel, history, diff, speculative transactions
- Negation and existential subgoals
- Pluggable `Storage` backend
