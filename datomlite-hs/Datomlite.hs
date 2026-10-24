-- |
-- A lightweight immutable database. The 'Db' is a plain value... 'commit'
-- hands back a new one and every past state stays queryable.
--
-- Records are the schema. A field's type says how it is stored and
-- 'Identity' names the fields that identify an entity:
--
-- @
-- \{\-\# LANGUAGE DeriveGeneric, DeriveAnyClass, DataKinds, TypeFamilies \#\-\}
-- \{\-\# LANGUAGE OverloadedRecordDot, OverloadedStrings \#\-\}
-- import Datomlite
--
-- data Department = Department { code :: Text, deptName :: Text }
--   deriving Generic
-- instance Entity Department where
--   type Identity Department = '[\"code\"]
--
-- data Employee = Employee
--   { email  :: Text             -- one value
--   , name   :: Text
--   , dept   :: Ref Department   -- another entity
--   , skills :: Set Text         -- many values
--   , nick   :: Maybe Text       -- optional
--   } deriving Generic
-- instance Entity Employee where
--   type Identity Employee = '[\"email\"]   -- unique, and what 'upsert' goes by
-- @
--
-- Writes are a list of 'Write's that commit all at once. A 'ref' names an
-- entity by its identity, so records in one list can point at each other:
--
-- @
-- commit_ db
--   [ Upsert (Department \"ENG\" \"Engineering\")
--   , Upsert (Employee \"m\@x.io\" \"Max\" (ref \@Department \"ENG\") mempty Nothing)
--   ]
-- @
--
-- A 'Tx' do-block ('insert', 'upsert', 'update', 'retract') is for steps that
-- need each other's results. 'commit' takes either.
--
-- Every read ('load', 'every', 'find' and queries) hands back a 'Stored'
-- record: @p.name@ reads a field and 'refOf' gives its 'Ref'.
--
-- Queries are do-blocks over those records:
--
-- @
-- query db $ do
--   e <- match \@Employee
--   d <- follow e.dept
--   guard (d.deptName == \"Engineering\")
--   pure (e.email, e.name)
-- @
--
-- A wrong field or type is a compile error. 'match' scans... 'follow' and
-- 'matchBy' go straight to the entity.
--
-- "Datomlite.Raw" goes around the schema when you want bare attributes and
-- entity ids.
module Datomlite
  ( -- * The database value
    Db
  , emptyDb
  , TxId
  , basisT
  , asOf
  , asOfTime
  , txTime
  , since

    -- * The log: every fact, ever
  , history
  , fromHistory
  , datoms
  , Datom (..)
  , Op (..)
  , refId

    -- * Schema: records are the schema
  , Entity (Identity, ename)
  , Ref
  , ref
  , refBy
  , ToValue (..)
  , FromValue (..)

    -- * Writing: build a 'Tx', then commit it
  , Tx
  , insert
  , insert_
  , upsert
  , upsert_
  , update
  , setField
  , retract
  , txDb
  , abort
  , Write (..)
  , Writes (Result)
  , commit
  , commit_
  , commitAt
  , TxError (..)
  , Attr
  , Value (..)
  , EntityId

    -- * Reading records
  , Stored
  , refOf
  , recordOf
  , load
  , every
  , find
  , findBy
  , LoadError (..)

    -- * Querying: do-blocks over records
  , Query
  , query
  , match
  , matchBy
  , follow
  , oneOf
  , closure
  , exists
  , none

    -- * Window functions over results
  , Window
  , window
  , rowNumber
  , rank
  , denseRank
  , lag
  , lead
  , whole
  , running

    -- * The IO edge
  , Conn
  , connect
  , snapshot
  , transact

    -- * Reacting to commits
  , listen
  , watch
  , awaitAfter
  , TxReport (..)
  ) where

import Datomlite.Core hiding (retract, transact)
import Datomlite.Schema
import Datomlite.Query
import Datomlite.Watch
import Datomlite.Window
