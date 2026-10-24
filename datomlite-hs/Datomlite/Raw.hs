-- |
-- Going around the schema: untyped writes, index probes and the shredding
-- machinery. 'transact' skips 'Datomlite.Identity' checks and typing, and
-- writes a lookup ref ('VLookup') as is... so prefer 'Datomlite.commit' unless
-- you mean it.
--
-- Its 'retract' and 'transact' are the raw ones, so import it qualified next
-- to "Datomlite": @import qualified Datomlite.Raw as Raw@.
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
import Datomlite.Schema hiding (retract, transact)
