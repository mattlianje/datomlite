# Changelog

## Unreleased

`Db` is now an immutable value: writes return a new db and leave the old one as it was. The mutable part moved to `Conn`, which you only
need for a shared db, listeners, live queries or storage.

Breaking:
- Writes on a db return the new db, and `transact` returns a `TxReport`
- `listen` and `watch` are on the conn now, and `listen` returns a subscription you can cancel
- `snapshot` and `withTx` are gone, since every db is already a snapshot
- `Db(storage, ...)` is now `Conn(storage)`. Use `Db.load(storage)` to read one into a plain db
- Writing to the conn from inside its own tx block throws. Use the block's builder instead

New:
- Live queries with `conn.watch`
- `Tx.fn` reads and writes in one commit, so concurrent updates no longer get lost
- Tx blocks and predicate writes can be built as values and applied later
- Tx reports include the db before and after, plus the datoms written
- Codecs for your own value types, derivable on case classes and enums
- A `Date` type for calendar days
- Optional refs
- Range and prefix filters use the index instead of scanning
- Conn writes are serialized and reads never block. A failed storage append aborts the tx
- Listeners see each commit once and in order, and one that throws doesn't affect the others

## 0.1.0 — 2026-05-18

Initial release.

- Core datomlite DB impl + Query builder impl
- Datalog joins, refs, aggregations, window functions, recursive rules
- Time-travel, history, diff, speculative transactions
- Negation and existential subgoals
- Pluggable `Storage` backend
