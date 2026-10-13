-- |
-- A lightweight immutable database. The 'Db' is a plain value... 'commit'
-- hands back a new one and every past state stays queryable.
--
-- Records are the schema. A field's type says how it is stored:
--
-- @
-- \{\-\# LANGUAGE DeriveGeneric, DeriveAnyClass, OverloadedRecordDot, OverloadedStrings \#\-\}
-- import Datomlite
--
-- data Department = Department { code :: Key Text, deptName :: Text }
--   deriving (Generic, Entity)
--
-- data Employee = Employee
--   { email  :: Key Text         -- unique
--   , name   :: Text             -- one value
--   , dept   :: Ref Department   -- another entity
--   , skills :: Set Text         -- many values
--   , nick   :: Maybe Text       -- optional
--   } deriving (Generic, Entity)
-- @
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
-- A wrong field or type is a compile error. 'Key' is dropped on read so
-- @e.email@ is just 'Data.Text.Text'. 'match' scans... 'follow' and 'matchBy'
-- go straight to the entity.
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
  , since

    -- * The log: every fact, ever
  , history
  , fromHistory
  , datoms
  , Datom (..)
  , Op (..)
  , refId

    -- * Schema: records are the schema
  , Entity (ename)
  , Key (..)
  , Ref
  , ToValue (..)
  , FromValue (..)

    -- * Writing: build a 'Tx', then commit it
  , Tx
  , new
  , save
  , update
  , setField
  , retractEntity
  , txDb
  , abort
  , commit
  , commit_
  , TxError (..)
  , Attr
  , Value (..)
  , EntityId

    -- * Reading records
  , load
  , loadAll
  , LoadError (..)

    -- * Querying: do-blocks over records
  , Query
  , query
  , match
  , matchBy
  , follow
  , oneOf
  , exists
  , none
  , Var
  , refOf
  , recordOf

    -- * The IO edge
  , Conn
  , connect
  , dbOf
  , commitIO

    -- * Reacting to commits
  , listen
  , watch
  , awaitAfter
  , TxReport (..)
  ) where

import Datomlite.Core
import Datomlite.Schema
import Datomlite.Query
import Datomlite.Watch
