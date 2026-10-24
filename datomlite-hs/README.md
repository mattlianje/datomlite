<div align="right">
  <sub><em>part of <a href="https://github.com/mattlianje/d4"><img src="https://raw.githubusercontent.com/mattlianje/d4/master/pix/cabinet6-mark-adaptive-light.svg" width="32" align="top" hspace="4">c6</a></em></sub>
</div>

# <img src="https://raw.githubusercontent.com/mattlianje/d4/refs/heads/master/pix/datomlite.png" width="60"> datomlite

**Lightweight, immutable database**

**datomlite** is a "DB-as-a-value" you can drop into your Haskell apps.

- Pure Haskell, zero-deps
- The `Db` is a plain immutable value
     - `commit` returns a new one
- Records are "the schema"
- Append-only log
     - Every past state stays queryable
- Queries are plain do-blocks, compile-time checked
     - Joins, refs, indexed lookups, negation, and recursive rules as recursive functions
- Easy time travel & speculative transactions
- IO only at the edge

```cabal
-- cabal.project
source-repository-package
  type:     git
  location: https://github.com/mattlianje/datomlite
  subdir:   datomlite-hs

-- your .cabal file
build-depends: datomlite
```

All you need:
```haskell
import Datomlite
```

Try it in your REPL:
```
cd datomlite-hs && cabal repl
```

## Contents
- [Quick start](#quick-start)
- [FAQ](#faq)
- [Updates](#updates)
- [Example schema](#example-schema)
- [Operations](#operations)
- [Identity and uniqueness](#identity-and-uniqueness)
- [Finding and lookup refs](#finding-and-lookup-refs)
- [Refs and cardinality](#refs-and-cardinality)
- [Value types](#value-types)
- [Optional fields](#optional-fields)
- [Tx blocks](#tx-blocks-steps-that-need-each-other)
- [Queries](#queries)
  - [Do-block joins](#do-block-joins)
  - [Indexed lookups](#indexed-lookups)
  - [Typed fields](#typed-fields)
  - [Reverse refs](#reverse-refs)
  - [Aggregations](#aggregations)
  - [Sort and paging](#sort-and-paging)
  - [Window functions](#window-functions)
- [Time travel](#time-travel-through-past-states)
- [History](#history-of-every-entity)
- [Diff](#diff-two-databases)
- [Negation](#negation-as-anti-join)
- [Existential subgoals](#existential-subgoals-semi-joins)
- [Rules](#rules)
  - [Recursive rules](#recursive-rules)
  - [Graphs with cycles](#graphs-with-cycles)
- [Sharing the db](#sharing-the-db-across-threads)
- [Listening and watching](#listening-and-watching)
- [Saving and restoring](#saving-and-restoring)
- [Build / Contribute](#build--contribute)

## Quick start

<details>
<summary>Pragmas and imports</summary>

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
import Control.Monad (guard)
import Data.Text (Text)
import GHC.Generics (Generic)
import Datomlite
```

</details>

```haskell
import Datomlite

-- Your records are the schema. `Identity` names the unique fields
data Person = Person { email :: Text, name :: Text }
  deriving (Show, Generic)

instance Entity Person where
  type Identity Person = '["email"]

-- Start from `emptyDb`. Commit a list of writes and get a new db back
Right db = commit_ emptyDb
  [ Insert (Person "m@x.io" "Matt")
  , Insert (Person "a@x.io" "Alice")
  ]

-- Your db feels like a collection
[ p.name | p <- every @Person db ]          -- ["Matt","Alice"]
length (every @Person db)                   -- 2
(.name) <$> find @Person "a@x.io" db        -- Just "Alice", by identity

-- Upsert by identity
Right db' = commit_ db [Upsert (Person "m@x.io" "Matthieu")]

-- Queries are plain do-blocks
query db' $ do
  p <- match @Person
  guard (p.email == "m@x.io")
  pure p.name
-- ["Matthieu"]

Right db'' = commit_ db' [Delete (ref @Person "m@x.io")]
```

## FAQ

**Is the `Db` really a value?**<br>
Yes. A `Db` is an ordinary immutable Haskell value, `commit` is a pure
function, and a snapshot is just the value you already hold.

**What is it under the hood?**<br>
An append-only log of `(entity, attribute, value)` datoms, plus in-memory
indices over the live projection. `GHC.Generics` shreds your records into
datoms on write and puts them back together on read, so queries work with your
original typed records.

**What indices does it maintain?**<br>
**EAVT** for live entity reads and **AVET** for value lookups, both derived from
the log and updated in lockstep with each commit. `load` and `follow` are a map
lookup, and identity checks are an AVET probe. `match @Employee` is a range scan
over the `Employee/…` attributes. `matchBy` is an AVET probe. `guard` filters
rows after they are produced, so reach for `matchBy` when you're matching a
field against a value.

**Does it support all of Datalog?**<br>
No. There is no Datalog engine. A query is a do-block in a list-like monad, so
joins are nested loops. Recursion is ordinary Haskell recursion, and `closure`
is the fixed point for graphs with cycles.

**Differences from Datomic / Datascript?**<br>
- Records are "the schema". There is no `:db/ident`, no attribute registration,
  and cardinality and uniqueness aren't separate facts.
- Queries are typed Haskell, not vectors of triples. The compiler checks every
  field access.
- Single process. No peer or transactor and no client API.

**Transaction guarantees?**<br>
- **Atomicity.** All the writes in a commit land together or none do. A
  `UniqueViolation`, an `abort` or a failed pattern gives you a `Left`, and
  there is no new `Db` at all.
- **Purity.** `commit db writes` is `Either TxError (result, Db)`. The input
  `Db` is untouched, so there is nothing to lock.
- **Linearizable commits.** Writers to a `Conn` take turns: `transact` runs the
  whole `Tx` against the current db, evaluates it, then swaps it in, so each
  commit sees the one before it. A `Tx` that throws (say, an `error` in a
  field) throws at its `transact` and leaves the `Conn` as it was.
- **Snapshot reads.** A `Db` never changes. `transact` hands back the one its
  commit made, and `snapshot` the one the `Conn` holds now.

**What if I change a record?**<br>
Old datoms stay as they were, and `load` reads them back into the new type.
- Adding a `Maybe` or `Set` field is free: old entities read it as `Nothing`
  or empty.
- Adding a required field, or changing a field's type, makes the old entities
  unreadable as the new record. `load` throws a `LoadError` (`MissingField` or
  `BadValue`) naming the attribute and entity, rather than quietly skipping
  them.
- `load` gives `Nothing` only when the entity has no facts of that type at
  all, say a retracted entity or a record whose fields are all `Nothing`.

**What can be an entity?**<br>
A record with one constructor and named fields, since each field's name is its
attribute. A positional constructor, a sum type or a fieldless type is a type
error that says what to change.

**What's missing?**<br>
Pull, a storage engine (see [Saving and restoring](#saving-and-restoring)),
pretty-printing and composite identities.

## Updates
Say we add two pugilists, Jet Li and Masvidal. That appends their datoms, and
the live projection picks them up:

```haskell
data Fighter = Fighter
  { fName    :: Text
  , lName    :: Text
  , nickname :: Maybe Text
  } deriving (Show, Generic)

instance Entity Fighter where
  type Identity Fighter = '["lName"]

Right db = commit_ emptyDb
  [ Insert (Fighter "Jorge" "Masvidal" (Just "Street Judas"))
  , Insert (Fighter "Jet"   "Li"       Nothing)
  ]
```

<p align="center">
  <img src="../pix/datomlite-masvi1.svg" width="550">
</p>

Unsurprisingly, we want to retract Masvidal:
```haskell
Right db' = commit_ db [Delete (ref @Fighter "Masvidal")]
map recordOf (every @Fighter db')
-- [Fighter {fName = "Jet", lName = "Li", nickname = Nothing}]
```
Masvi is gone from the live projection, but not from the log. `history db'` has
all 8 datoms (5 asserts, 3 retracts) and `datoms db'` has Jet Li's 2.

<p align="center">
  <img src="../pix/datomlite-masvi2.svg" width="550">
</p>

## Example schema
We'll use these vanilla domain entities throughout for example purposes...

```haskell
data Department = Department { code :: Text, name :: Text }
  deriving (Show, Generic)
instance Entity Department where
  type Identity Department = '["code"]

data Skill = Skill { name :: Text }
  deriving (Show, Generic)
instance Entity Skill where
  type Identity Skill = '["name"]

data Order = Order { buyer :: Text, item :: Text, price :: Int }
  deriving (Show, Generic, Entity)

data Employee = Employee
  { email    :: Text
  , name     :: Text
  , dept     :: Ref Department
  , salary   :: Int
  , skills   :: Set (Ref Skill)
  , nickname :: Maybe Text
  } deriving (Show, Generic)
instance Entity Employee where
  type Identity Employee = '["email"]
```

...and this db:

```haskell
Right db = commit_ emptyDb
  [ Insert (Department "ENG" "Engineering")
  , Insert (Department "SAL" "Sales")
  , Insert (Skill "Scala")
  , Insert (Skill "Haskell")
  , Insert (Employee "m@x.io" "Matt"  (deptRef "ENG") 120000 (skillRefs ["Scala", "Haskell"]) (Just "Matty"))
  , Insert (Employee "a@x.io" "Alice" (deptRef "ENG") 110000 (skillRefs ["Scala"])            Nothing)
  , Insert (Employee "b@x.io" "Bob"   (deptRef "SAL") 150000 mempty                           Nothing)
  , Insert (Order "m@x.io" "book" 20)
  , Insert (Order "a@x.io" "lamp" 90)
  , Insert (Order "b@x.io" "chair" 60)
  ]
  where
    deptRef   = ref @Department
    skillRefs = Set.fromList . map (ref @Skill)

-- Engineering's Ref, for the examples below
Just eng = refOf <$> find @Department "ENG" db
```

`ref @Department "ENG"` points at a department by its code, so employees can
reference it in the same list ([lookup refs](#finding-and-lookup-refs)).

Records can share field names like `name` (`DuplicateRecordFields`). `e.name`
knows which record it's reading, and so does `setField`.

The snippets from here on assume these:

<details>
<summary>Pragmas and imports</summary>

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
import Control.Applicative ((<|>))
import Control.Monad (guard, when)
import Data.Foldable (for_, traverse_)
import Data.List (sortOn, (\\))
import qualified Data.Map.Strict as Map
import Data.Ord (Down (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Time (getCurrentTime)
import GHC.Generics (Generic)
import Datomlite
```

</details>

## Operations
A commit takes a list of writes and applies them in order, all-or-nothing.
Reads take a `Db` value, which never changes under you.

Write:
```
Insert r               insert a record as a new entity
Upsert r               insert, or update the entity already holding r's Identity
Update ref f           change an entity with a function on its record
Delete ref             retract every fact the entity holds
setField @"f" v        a record -> record function that sets field f
commit_ db [w, ...]    Either TxError db'
commit db [w, ...]     Either TxError ((), db')
```

When a step needs an earlier step's result, write a [`Tx`
do-block](#tx-blocks-steps-that-need-each-other) instead. `commit` takes either.

Read:
```
load ref db          Maybe (Stored A)
every @A db          [Stored A], in creation order
find @A v db         Maybe (Stored A), by its Identity
query db q           [a], run a Query
```

Reads give you a `Stored A`, the record plus its `Ref`: `p.name` for a field,
`refOf p` for the ref, `recordOf p` for the record.

```haskell
-- Adding
Right db1 = commit_ db [Insert (Employee "c@x.io" "Carol" eng 100000 mempty Nothing)]
Right db2 = commit_ db1
  [ Insert (Employee "d@x.io" "Dan" eng 90000 mempty Nothing)
  , Insert (Employee "e@x.io" "Eve" eng 95000 mempty Nothing)
  ]

-- Lookups
(.name) <$> find @Employee "c@x.io" db2 -- Just "Carol"
map recordOf (every @Employee db2)      -- [Employee]
query db2 $ do                          -- [Employee]
  e <- match @Employee
  guard (e.email == "m@x.io")
  pure (recordOf e)

-- Retract
Right db3 = commit_ db2 [Delete (ref @Employee "c@x.io")]

-- Quick checks: results are lists
not (null (query db3 (byEmail "m@x.io")))  -- True
length (every @Employee db3)               -- 5
```

A `Query` is a value, so a reusable lookup is just a function. `matchBy` finds
the employee through the index instead of scanning them all:

```haskell
byEmail :: Text -> Query (Ref Employee)
byEmail em = refOf <$> matchBy @Employee @"email" em
```

`basisT db` is the last transaction in a db.

## Identity and uniqueness
To keep a field unique, name it in the record's `Identity`:
```
type Identity t = '["f", ...]   each field unique on its own (default: none)
Upsert r                        find by identity and write only the difference
ename                           override the attribute prefix
```

The record stays plain. `email` is just a `Text`, whether you build it, `load`
it or read `e.email` in a query. A typo is a compile error:
`datomlite: Identity User names "emial" but User has no such field`.

Each field in the list is unique on its own, like Datomic's
`:db.unique/identity`. A duplicate, against the db or within the same commit,
fails with `UniqueViolation` and writes nothing. A `Maybe` identity only counts
when it's `Just`.

```haskell
data User = User
  { email  :: Text
  , handle :: Text
  , name   :: Text
  } deriving (Show, Generic)

instance Entity User where
  type Identity User = '["email", "handle"]

Right udb = commit_ emptyDb [Insert (User "a@x.io" "shared" "Alice")]

commit_ udb [Insert (User "b@x.io" "shared" "Bob")]
-- Left (UniqueViolation { uvAttr = "User/handle", uvValue = VText "shared"
--                       , uvEntities = [EntityId 1,EntityId 2] })
```

`uvEntities` lists the entities that would share the value, oldest first. The
check looks at the commit's end result, so retracting an entity and reusing its
identity in one commit is fine.

`Upsert` updates the entity in place, so refs to it stay valid. On the example
db:
```haskell
commit_ db [Upsert (Department "ENG" "Platform")]
-- Matt's and Alice's dept now reads "Platform"
```

The same record name in two modules collides on the prefix. Override `ename`:
```haskell
instance Entity Person where
  type Identity Person = '["email"]
  ename _ = "a.Person"
```

## Finding and lookup refs
`find` reads an entity by its identity. `ref` points at it without reading
anything:
```
find @A v db           Maybe (Stored A), the A whose Identity is v
ref @A v               a Ref A to whichever entity holds v
findBy @A @"f" v db    find, for a record with several identities
refBy @A @"f" v        ref, likewise
```

```haskell
find @Department "ENG" db             -- Just (Stored (Ref (EntityId 1)) (Department ...))
findBy @User @"handle" "shared" udb   -- Just (Stored ...)
```

A `ref` works anywhere a `Ref` does. It's resolved on commit, so it can point at
something written earlier in the same list:

```haskell
Right db2 = commit_ db
  [ Upsert (Department "OPS" "Operations")
  , Upsert (Skill "Nix")
  , Upsert (Employee "o@x.io" "Olga" (ref @Department "OPS") 105000
                     (Set.fromList [ref @Skill "Nix", ref @Skill "Haskell"]) Nothing)
  ]
```

`load`, `follow`, `matchBy`, `Update` and `Delete` all take one:

```haskell
commit_ db
  [ Update (ref @Employee "m@x.io") (setField @"salary" 125000)
  , Delete (ref @Employee "b@x.io")
  ]

query db $ do
  e <- matchBy @Employee @"dept" (ref @Department "ENG")
  pure e.name
-- ["Matt","Alice"]
```

- **Typed.** The value has the field's type (the inner type for `Maybe` and
  `Set`). `find` on a record with no identity, or several, is a type error.
- **Writes need a match.** A `ref` to nothing fails the commit with
  `UnknownIdentity`. A read just comes back empty.
- **Stored as an id.** The datom holds the entity id, so `e.dept` reads back as
  a normal `Ref`. Refs compare by what they hold, so a `ref` never equals a
  stored one. Compare with `refOf d`, not `ref @Department "ENG"`.

## Refs and cardinality
Cardinality is a property of the field's type, not a separate declaration:

```
a          one datom
Maybe a    one datom on Just, nothing on Nothing
Set a      one datom per element
Ref t      one datom, value is the target's entity id
```

`Ref t` is a scalar like any other, so `Maybe (Ref t)` and `Set (Ref t)` just
work. Many-valued fields are `Set`s, since facts have no order or duplicates. An
`[a]` field is a type error. `follow` walks a ref and `oneOf` ranges over a
`Set` or `Maybe`:

```haskell
query db $ do
  e <- match @Employee
  d <- follow e.dept
  guard (d.name == "Engineering")
  s <- follow =<< oneOf e.skills
  pure (e.email, s.name)
-- [("m@x.io","Scala"),("m@x.io","Haskell"),("a@x.io","Scala")]
```

You can also compare a ref field to a `Ref` you already have: `guard (e.dept ==
eng)`.

## Value types
A field holds one of these, bare or inside `Maybe` or `Set`:

```
Text, Int, Integer, Double, Bool
ByteString        strict, from bytestring
UTCTime, Day      from time
Ref t             another entity
your enums        stored as the constructor's name
your newtypes     stored as the type they wrap
```

Enums and newtypes take one deriving line each:

<details>
<summary>Extra pragmas and imports</summary>

```haskell
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import Data.Time (Day)
```

</details>

```haskell
data Level = Junior | Senior
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToValue, FromValue)  -- "Junior" / "Senior"

newtype Email = Email Text
  deriving newtype (Show, Eq, Ord, ToValue, FromValue)

data Hire = Hire { email :: Email, level :: Level, started :: Day }
  deriving (Show, Generic, Entity)
```

`Ord` is only needed to put them in a `Set`. Rename an enum constructor and
entities holding the old name throw `BadValue` on read. A constructor with
fields can't derive `ToValue`; make it its own entity and point at it with a
`Ref`. `NaN` is a normal value: store it, match it, retract it.

## Optional fields
`Maybe a` writes a datom only on `Just`. `Nothing` is absence, not a stored
null.

```haskell
query db $ do
  e <- match @Employee
  guard (e.nickname == Just "Matty")
  pure e.email
-- ["m@x.io"]

query db $ do
  e <- match @Employee
  guard (e.nickname == Nothing)
  pure e.email
-- ["a@x.io","b@x.io"]
```

`oneOf` gives a row only when there's a value:

```haskell
query db $ do
  e <- match @Employee
  n <- oneOf e.nickname
  pure (e.email, n)
-- [("m@x.io","Matty")]
```

## Tx blocks: steps that need each other
A list covers most writes. Use a `Tx` do-block when a step needs an earlier
result, the db so far, or a way to bail. `commit` and `transact` take either:
```
insert r / upsert r   Insert / Upsert, returning the entity's Ref
update ref f          Update
retract ref           Delete
txDb                  the in-progress db, earlier steps included
abort msg             give up with Aborted msg
Just x <- ...         a failed pattern aborts too (MonadFail)
commit db tx          Either TxError (the block's result, db')
```

Steps run in order, share one tx id, and commit all-or-nothing. The block
returns what its last step returns:

```haskell
Right (carol, _) = commit db $ insert (Employee "c@x.io" "Carol" eng 100000 mempty Nothing)
-- carol :: Ref Employee
```

```haskell
Right db' = commit_ db $ do
  now <- txDb
  for_ (query now (byEmail "m@x.io")) $ \r ->
    update r (setField @"name" "Matthieu")
  insert (Order "m@x.io" "book" 20)
```

`update` writes only the difference, so a single-valued field gets replaced, not
doubled. It takes any `a -> a`. Use `setField` when two records share a field
name, since `e { name = … }` is ambiguous then. Otherwise plain record update is
fine (see `balance` below). `setField`s compose with `.`.

`txDb` sees everything so far, so read-modify-write runs atomically inside the
transaction:

```haskell
data Account = Account { owner :: Text, balance :: Int }
  deriving (Show, Generic)

instance Entity Account where
  type Identity Account = '["owner"]

transfer :: Ref Account -> Ref Account -> Int -> Tx ()
transfer from to amount = do
  Just a <- load from <$> txDb   -- no such account: aborts
  when (a.balance < amount) $ abort "insufficient funds"
  update from (\x -> x { balance = x.balance - amount })
  update to   (\x -> x { balance = x.balance + amount })

Right adb = commit_ emptyDb [Insert (Account "alice" 100), Insert (Account "bob" 0)]

alice = ref @Account "alice"
bob   = ref @Account "bob"

commit_ adb (transfer alice bob 30)   -- Right adb', alice 70 and bob 30
commit_ adb (transfer alice bob 500)
-- Left (Aborted "insufficient funds")
```

Writes and `Tx`s are just values, so you can build them now and commit later:
```haskell
orders :: [Write]
orders = [Insert (Order "m@x.io" "book" 20), Insert (Order "m@x.io" "pen" 5)]

commit_ db orders
```

A failed transaction gives a `TxError`:
```
UniqueViolation attr v es    an Identity value the entities es would share
UnknownIdentity attr v       a ref nothing holds
UnknownEntity eid            update on an entity with no facts
Aborted msg                  abort, or a failed pattern
```

## Queries
A query is an ordinary do-block. The rest (`guard`, `==`, `pure`, `<|>`) is
plain Haskell. These words are new:
```
match @A              every entity of type A
matchBy @A @"f" v     the A's whose field f holds v, via the index
follow ref            the entity a Ref (or a lookup ref) points at
oneOf xs              each value of a Set or Maybe field
exists q / none q     keep the row if subquery q has rows / has none
closure step x        everything reachable by repeating step, safe on cycles
```

### Do-block joins
Stitch entities together on a shared field, with no DSL. Rows are whatever you
`pure`: a tuple, a record, a `Ref`.

```haskell
query db $ do
  e <- match @Employee
  o <- match @Order
  guard (o.buyer == e.email)
  pure (e.name, o.item)
-- [("Matt","book"),("Alice","lamp"),("Bob","chair")]
```

> [!NOTE]
> A `match` plus a `guard` is a nested loop. `follow` and `matchBy` are the
> exceptions: both are direct index lookups.

### Indexed lookups
`matchBy @A @"f" v` gives the `A`s whose field `f` holds `v`, through the index.
The field and value type are checked like `e.name`. `Maybe` and `Set` fields
match the value inside, and a `Ref` field matches its target:

```haskell
query db $ do
  e <- match @Employee
  o <- matchBy @Order @"buyer" e.email
  pure (e.name, o.item)
-- [("Matt","book"),("Alice","lamp"),("Bob","chair")]

query db $ do
  s <- matchBy @Skill @"name" "Scala"
  e <- matchBy @Employee @"skills" (refOf s)
  pure e.name
-- ["Matt","Alice"]
```

A typo is caught at compile time: `datomlite: Employee has no field "emial"`.

### Typed fields
`e.name` reads the record's own field with its declared type (`e.nickname` is a
`Maybe Text`), so `e.emial` or `e.name == 5` won't compile. `recordOf e` is the
whole record, and `refOf e` is its `Ref`.

### Reverse refs
`refOf` is how you join the other way, from a department to the employees
pointing at it. With `matchBy` both steps are index lookups:

```haskell
query db $ do
  d <- matchBy @Department @"code" "ENG"
  e <- matchBy @Employee @"dept" (refOf d)
  pure e.name
-- ["Matt","Alice"]
```

### Aggregations
Results are lists, so rollups are plain Haskell over them:

```haskell
Map.fromListWith (+) $ query db $ do
  e <- match @Employee
  o <- matchBy @Order @"buyer" e.email
  pure (e.name, o.price)
-- fromList [("Alice",90),("Bob",60),("Matt",20)]
```

### Sort and paging
The same goes for ordering and slicing:

```haskell
take 2 . sortOn (Down . snd) $ query db $ do
  e <- match @Employee
  guard (e.salary > 100000)
  pure (e.name, e.salary)
-- [("Bob",150000),("Matt",120000)]
```

### Window functions
SQL's `rank() OVER (PARTITION BY ... ORDER BY ...)`, as plain functions over
the result list:

```
window part ord w rows   group by part, sort by ord, run w on each group
rowNumber                1, 2, 3...
rank                     ties share a rank and the next one skips: 1, 1, 3
denseRank                ties share a rank with no skip: 1, 1, 2
lag n f / lead n f       f of the row n before / after, Nothing past the edge
whole f                  f of the whole group, on each row
running f                f of the group up to this row
```

```haskell
pay = query db $ do
  e <- match @Employee
  d <- follow e.dept
  pure (d.name, e.name, e.salary)

deptOf   (d, _, _) = d
salaryOf (_, _, s) = s

window deptOf (Down . salaryOf) ((,) <$> rank <*> whole (sum . map salaryOf)) pay
-- [ (("Engineering","Matt",120000),  (1,230000))
-- , (("Engineering","Alice",110000), (2,230000))
-- , (("Sales","Bob",150000),         (1,150000)) ]

-- top earner per dept
[ (d, n) | ((d, n, _), 1) <- window deptOf (Down . salaryOf) rowNumber pay ]
-- [("Engineering","Matt"),("Sales","Bob")]
```

Windows combine with `<*>` like above. Use `const ()` as `part` for one group.

## Time travel through past states
Queries run on any `Db` value, so you can read any past state or branch off a
speculative transaction. Either way, the db you started from is untouched.

```
basisT db       the tx a db value reflects
asOf t db       rewind to tx t
asOfTime t db   rewind to a wall-clock time
txTime tx db    when tx was committed
since t db      only the facts added after tx t
db              a snapshot is the value you already hold
commit db tx    a what-if branch is a commit you don't keep
```

```haskell
names :: Query Text
names = do
  e <- match @Employee
  pure e.name

-- db' is db after the "Matthieu" rename from Tx blocks
query db' names                      -- ["Matthieu","Alice","Bob"]
query (asOf (basisT db) db') names   -- ["Matt","Alice","Bob"]
query (since (basisT db) db2) names  -- ["Carol","Dan","Eve"]
```

`transact` stamps each transaction with the clock. `commit` is pure, so it
doesn't. `commitAt t` uses a time you give it:

```haskell
conn <- connect db
before <- getCurrentTime
Right (_, now) <- transact conn [Insert (Employee "d@x.io" "Dan" eng 90000 mempty Nothing)]

length (every @Employee (asOfTime before now))  -- 3, before Dan
length (every @Employee now)                    -- 4
```

An unstamped transaction counts as happening at the stamp before it, so the pure
seed above shows up at any time. Times never go backwards, even if the clock
does. Each is logged as a `db/txInstant` datom, so it's in `history` and
survives a save.

A `since` view only has the newer facts, so an entity created before `t` and
changed after is a fragment there. Typed reads skip fragments. Use `datoms` on
the view for every fact.

Snapshot vs what-if:
```haskell
let whatIf = commit_ db2 [Insert (Employee "z@x.io" "Zed" eng 1 mempty Nothing)]

length (every @Employee db2)           -- 6
length . every @Employee <$> whatIf    -- Right 7, includes Zed
length (every @Employee db2)           -- 6, still
```

## History of every entity
`history` gives the full log of datoms, and `refId` ties a `Ref` to its trail.

```
history db             every datom, oldest first
datoms db              just the live facts
refId ref              a Ref's EntityId, to filter a trail by
```

Each `Datom` carries its tx and an `Assert` or `Retract`, and nothing is
overwritten. A trail survives retraction:

```haskell
Just carol = refOf <$> find @Employee "c@x.io" db2

[ (dAttr d, dValue d, dTx d, dOp d) | d <- history db3, dEntity d == refId carol ]
-- [ ("Employee/email",  VText "c@x.io",    TxId 2, Assert)
-- , ("Employee/name",   VText "Carol",     TxId 2, Assert)
-- , ("Employee/dept",   VRef (EntityId 1), TxId 2, Assert)
-- , ("Employee/salary", VInt 100000,       TxId 2, Assert)
-- , ("Employee/dept",   VRef (EntityId 1), TxId 4, Retract)
-- , ... ]
```

To skip the schema, import `Datomlite.Raw`: stringly-typed `transact` and
`assert`/`retract`, plus index probes like `lookupValue` and `entitiesWith`. Raw
writes skip type and uniqueness checks. Import it qualified, since its
`transact` and `retract` clash with the main ones.

## Diff two databases
Two db values diff like two lists. An unchanged fact is the same `Datom` in
both, so a diff is list difference over `datoms`:

```haskell
-- db' is db after the Tx blocks example: a rename plus a new order
[ dValue d | d <- datoms db' \\ datoms db ]   -- added
-- [VText "Matthieu",VText "m@x.io",VText "book",VInt 20]
[ dValue d | d <- datoms db \\ datoms db' ]   -- retracted
-- [VText "Matt"]
```

## Negation as anti-join
`none` keeps a row only when a subquery comes back empty. The subquery runs
against the same db as the query around it, so this works unchanged on an
`asOf` or a what-if:

```haskell
-- employees with no orders
query db2 $ do
  e <- match @Employee
  none (matchBy @Order @"buyer" e.email)
  pure e.name
-- ["Carol","Dan","Eve"]
```

## Existential subgoals (semi-joins)
Sometimes you only need to know a match exists, not what it is. `exists` keeps
the row once, however many matches there are:

```haskell
-- orders whose buyer exists as a registered Employee
query db $ do
  o <- match @Order
  exists (byEmail o.buyer)
  pure o.item
-- ["book","lamp","chair"]
```

## Rules
A rule is just a function returning a `Query`. Call it anywhere in a do-block.
`<|>` is disjunction (either branch holds), and a rule that calls itself is
recursive.

### Recursive rules

Let's imagine we have a hierarchy of officers...
<p align="center">
  <img src="../pix/datomlite-tree-ex1.svg" width="500">
</p>

```haskell
data Off = Off
  { officer :: Text
  , boss    :: Maybe (Ref Off)
  } deriving (Generic)

instance Entity Off where
  type Identity Off = '["officer"]

Right db = commit_ emptyDb
  [ Insert (Off "Napoléon" Nothing)
  , Insert (Off "Davout"   (under "Napoléon"))
  , Insert (Off "Ney"      (under "Napoléon"))
  , Insert (Off "Murat"    (under "Napoléon"))
  , Insert (Off "Friant"   (under "Davout"))
  , Insert (Off "Gudin"    (under "Davout"))
  , Insert (Off "Lasalle"  (under "Murat"))
  , Insert (Off "Marchand" (under "Ney"))
  ]
  where under = Just . ref @Off

-- a direct report, or anyone below one
subordinates :: Stored Off -> Query (Stored Off)
subordinates o = do
  s <- matchBy @Off @"boss" (refOf o)
  pure s <|> subordinates s

-- their boss, or anyone above them
bosses :: Stored Off -> Query (Stored Off)
bosses o = do
  b <- follow =<< oneOf o.boss
  pure b <|> bosses b

named :: Text -> Query (Stored Off)
named = matchBy @Off @"officer"
```

**Subordinates of "Davout":**
<p align="center">
  <img src="../pix/datomlite-tree-ex2.svg" width="500">
</p>

```haskell
query db $ do
  s <- subordinates =<< named "Davout"
  pure s.officer
-- ["Friant","Gudin"]
```

**"Lasalle"'s reporting line:**
<p align="center">
  <img src="../pix/datomlite-tree-ex3.svg" width="500">
</p>

```haskell
query db $ do
  b <- bosses =<< named "Lasalle"
  pure b.officer
-- ["Murat","Napoléon"]
```

In SQL: recursive CTE.

```sql
WITH RECURSIVE ancestor(anc, desc) AS (
  SELECT boss, name FROM off WHERE boss IS NOT NULL
  UNION
  SELECT a.anc, o.name
  FROM ancestor a JOIN off o ON o.boss = a.desc
)
SELECT desc FROM ancestor WHERE anc = 'Napoleon';
```

> [!NOTE]
> This is plain recursion, so it terminates on trees and DAGs. On a graph with
> cycles it would loop forever... use `closure` there.

### Graphs with cycles
`closure step x` is everything reachable from `x` in one or more steps, each
entity once. It's the fixed point of the rule, so cycles are fine.

#### Directed graph (Roads)

<p align="center">
  <img src="../pix/datomlite-roads.svg" width="350">
</p>

```haskell
data Place = Place { code :: Text } deriving (Generic)
data Road  = Road { from :: Ref Place, to :: Ref Place } deriving (Generic, Entity)

instance Entity Place where
  type Identity Place = '["code"]

Right db = commit_ emptyDb $
     [ Insert (Place c) | c <- ["PAR", "LYO", "MAR", "NCE", "BOR"] ]
  ++ [ Insert (Road (at a) (at b))
     | (a, b) <- [("PAR", "LYO"), ("LYO", "MAR"), ("MAR", "PAR") {- cycle -}, ("LYO", "NCE"), ("PAR", "BOR")] ]
  where at = ref @Place

-- one road away
next :: Stored Place -> Query (Stored Place)
next p = do
  r <- matchBy @Road @"from" (refOf p)
  follow r.to

-- everywhere reachable from Paris. The cycle means PAR reaches itself
query db $ do
  t <- closure next =<< matchBy @Place @"code" "PAR"
  pure t.code
-- ["LYO","BOR","MAR","NCE","PAR"]
```

#### Undirected graph (Friends)
A friendship is stored once and `knows` reads it both ways with `<|>`.
Seven people, six edges, two components:

<p align="center">
  <img src="../pix/datomlite-friends.svg" width="500">
</p>

```haskell
data Person = Person { handle :: Text } deriving (Generic)
data Friend = Friend { a :: Ref Person, b :: Ref Person } deriving (Generic, Entity)

instance Entity Person where
  type Identity Person = '["handle"]

Right db = commit_ emptyDb $
     [ Insert (Person h) | h <- ["alice", "bob", "carol", "dave", "erin", "frank", "grace"] ]
  ++ [ Insert (Friend (who x) (who y))
     | (x, y) <- [ ("alice", "bob"), ("alice", "carol"), ("bob", "dave")
                 , ("carol", "dave"), ("dave", "erin"), ("frank", "grace") ] ]
  where who = ref @Person

knows :: Stored Person -> Query (Stored Person)
knows x = (follow . (.b) =<< matchBy @Friend @"a" (refOf x))
      <|> (follow . (.a) =<< matchBy @Friend @"b" (refOf x))

handleIs :: Text -> Query (Stored Person)
handleIs = matchBy @Person @"handle"

-- Alice's cluster. She's in it too, through any friend and back
query db $ do
  y <- closure knows =<< handleIs "alice"
  pure y.handle
-- ["bob","carol","dave","alice","erin"]

-- Frank is in a separate component
query db $ do
  y <- closure knows =<< handleIs "alice"
  guard (y.handle == "frank")
-- []
```

Results come out nearest first. `closure` is lazy so `exists (closure ...)`
stops at the first hit.

## Sharing the db across threads
`Db` is a plain value, so pass it anywhere. When threads need one shared current
db, use a `Conn`, a mutable cell holding it. `transact` is `commit` on that
cell. It takes the same writes, runs the **same** checks and returns the same
result and new db. The `Conn` then holds that db:

```
commit   db   w     Either TxError (r, Db)        pure, the db is yours
transact conn w     IO (Either TxError (r, Db))   the Conn holds the new db
connect db          IO Conn
snapshot conn       IO Db, whatever the Conn holds right now
```

`r` is `()` for a list and the block's result for a `Tx`.

```haskell
conn <- connect emptyDb
Right ((), db) <- transact conn
  [ Insert (Department "ENG" "Engineering")
  , Insert (Employee "m@x.io" "Matt" (ref @Department "ENG") 120000 mempty Nothing)
  ]

(.name) <$> find @Employee "m@x.io" db  -- Just "Matt"
```

The `db` you get back is the one your commit made, even if another thread has
committed since. `snapshot conn` reads whatever is current, other threads'
commits included.

Everything else stays pure. Hand `db` to any function that reads.

## Listening and watching
A `Conn` holds its db in a `TVar`, so a subscriber is just a thread that waits
for the next commit. Writers never wait on subscribers:

```
listen conn f        IO (IO ()), f gets one TxReport per commit
watch conn q f       IO (IO ()), f gets q's result, up front and on change
awaitAfter conn t    STM Db, blocks until a commit newer than t
```

```haskell
stop <- listen conn $ \r ->
  putStrLn (show (trTxId r) <> ": " <> show (length (trDatoms r)) <> " datoms")

_ <- watch conn (length . every @Employee) $ \n ->
  putStrLn (show n <> " employees")

stop   -- unsubscribe
```

- **In order, none missed.** Each subscriber sees commits in tx order. A
  listener that falls behind catches up from the log, one report per commit.
- **Isolated.** Callbacks run on the subscriber's own thread, so they may read
  or commit. An exception is reported on stderr and never reaches `transact`.
- **`watch` shows the latest result.** `q` runs on the newest db, so if commits
  land faster than `f` keeps up, it skips the results in between.
- **Asynchronous.** A callback may not have run yet when `transact` returns.
  To wait for a commit yourself, use `awaitAfter`. It is plain STM, so it
  composes with `orElse` and timeouts.

## Saving and restoring
The log is the database. Save its datoms and `fromHistory` rebuilds the `Db`,
past included. `Datom`'s `Show` and `Read` round-trip, so one datom per line is
a file format with no extra deps:

```
history db           [Datom], every fact, oldest first
fromHistory datoms   Db, rebuilt from them
```

<details>
<summary>Extra imports</summary>

```haskell
import System.IO (readFile')
```

</details>

```haskell
restore :: FilePath -> IO Db
restore path = fromHistory . map read . lines <$> readFile' path

-- open the log (an empty file for a new db), then append each commit to it
conn <- connect =<< restore "app.log"
stop <- listen conn $ \r -> appendFile "app.log" (unlines (map show (trDatoms r)))
```

A restored db picks up where the old one stopped: same entities, same past for
`asOf` and `asOfTime`, and new ids never reuse one from the log.

- **`listen` is asynchronous**, so a crash right after a `transact` can lose
  the last commits. If each commit must be on disk before you move on, append
  `trDatoms` yourself after `transact` returns.
- **`Show` is simple, not fast.** For a big log, write your own encoding. Every
  `Datom`, `Value` and id constructor is exported (ids from `Datomlite.Raw`).

## Build / Contribute
PRs welcome, but please keep it to boot-library deps...
```
make build
make test
make repl
```
