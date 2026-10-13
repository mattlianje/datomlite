-- |
-- Going around the schema: untyped writes, index probes and the shredding
-- machinery. 'transact' skips 'Datomlite.Key' checks and typing... so prefer
-- 'Datomlite.commit' unless you mean it.
module Datomlite.Raw
  ( -- * Bare ids
    EntityId (..)
  , TxId (..)

    -- * Raw transactions
  , TxData
  , assert
  , retract
  , TxReport (..)
  , transact
  , freshEntity

    -- * Index reads
  , entityAttrs
  , lookupValue
  , lookupValues
  , entitiesWith
  , entitiesInNamespace
  , uniqueAt

    -- * From records to facts
  , entityFields
  , Entity (..)
  , Field (..)
  , Shape (..)

    -- * Connections
  , atomicUpdate
  ) where

import Datomlite.Core
import Datomlite.Schema
