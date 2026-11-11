# Changelog

## 0.1.0.0 (unreleased)

Initial release.

- Immutable database value: an append-only log of datoms, indexed two ways
- Your plain Haskell records are the schema. Unique fields are declared at the type level
- Bad schemas fail at compile time
- Atomic transactions, either as a list of writes or a do-block
- Lookup refs, so you can point at an entity by its unique field
- Queries are do-blocks, with joins, negation and recursion
- Window functions (rank, lag, running totals...)
- Time travel: read the db as it was at any transaction or point in time, or see a fact's history
- A connection type for the IO side, with change listeners
- The whole library is one file. Copy it into your project and you're done
