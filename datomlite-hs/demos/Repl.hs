-- setup
{-# LANGUAGE DataKinds, DeriveGeneric, DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot, OverloadedStrings, TypeApplications, TypeFamilies #-}
import Data.Text (Text)
import GHC.Generics (Generic)

import Datomlite

-- Records are the schema
data Fighter = Fighter { name :: Text, style :: Text } deriving (Show, Generic)
instance Entity Fighter where type Identity Fighter = '["name"]

data Bout = Bout { code :: Text, winner, loser :: Ref Fighter } deriving (Show, Generic)
instance Entity Bout where type Identity Bout = '["code"]

-- A db is a value. commit_ hands back a new one
li     = ref @Fighter "Jet Li"
gegard = ref @Fighter "Gegard Mousasi"
rory   = ref @Fighter "Rory MacDonald"
Right db = commit_ emptyDb
  [ Insert (Fighter "Jet Li" "Wushu")
  , Insert (Fighter "Gegard Mousasi" "Mixed")
  , Insert (Fighter "Rory MacDonald" "Pankration")
  , Insert (Bout "B1" li gegard)
  , Insert (Bout "B2" gegard rory)
  ]

-- Joins are plain do-blocks, checked at compile time
query db $ do
  b <- match @Bout
  w <- follow b.winner
  l <- follow b.loser
  pure (w.name, l.name)

-- Recursive rules are recursive functions
beat f = matchBy @Bout @"winner" (refOf f) >>= follow . (.loser)

-- Everyone Jet Li beat, directly or not
[ f.name | f <- query db (closure beat =<< matchBy @Fighter @"name" "Jet Li") ]

-- Speculative: what if Rory beat Jet Li? A branch, db is untouched
Right whatIf = commit_ db [Insert (Bout "B3" rory li)]

[ [ b.code | b <- every @Bout d ] | d <- [db, whatIf] ]

-- Upsert by identity
Right db' = commit_ db [Upsert (Fighter "Gegard Mousasi" "Kickboxing")]

-- Every past state stays queryable: now vs. as of the old db
[ (.style) <$> find @Fighter "Gegard Mousasi" d | d <- [db', asOf (basisT db) db'] ]
