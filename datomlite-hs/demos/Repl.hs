-- setup
{-# LANGUAGE DataKinds, DeriveGeneric, DuplicateRecordFields, FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot, OverloadedStrings, TypeApplications, TypeFamilies #-}
import Control.Monad (guard)
import Data.List (sortOn)
import Data.Ord (Down (..))
import Data.Text (Text)
import GHC.Generics (Generic)
import System.IO (readFile')
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Datomlite

-- Records are the schema
data Fighter = Fighter
  { name    :: Text
  , style   :: Text
  , country :: Text
  } deriving (Show, Generic)

instance Entity Fighter where
  type Identity Fighter = '["name"]   -- the identity field

data Bout = Bout
  { code   :: Text
  , red    :: Ref Fighter
  , blue   :: Ref Fighter
  , winner :: Ref Fighter
  } deriving (Show, Generic)

instance Entity Bout where
  type Identity Bout = '["code"]

li     = Fighter "Jet Li"         "Wushu"      "China"
gegard = Fighter "Gegard Mousasi" "Mixed"      "Netherlands"
rory   = Fighter "Rory MacDonald" "Pankration" "Canada"

-- A ref names a fighter by identity
bout :: Text -> Fighter -> Fighter -> Fighter -> Bout
bout c r b w = Bout c (to r) (to b) (to w)
  where to f = ref @Fighter f.name

-- Your db is a plain value. All-or-nothing writes hand back a new one
db0 = emptyDb
Right db = commit_ db0
  [ Insert li, Insert gegard, Insert rory
  , Insert (bout "B1" li     gegard li)
  , Insert (bout "B2" li     rory   li)
  , Insert (bout "B3" gegard rory   gegard)
  , Insert (bout "B4" rory   li     li)
  ]

-- Reads feel like a list
length (every @Fighter db)                                  -- 3
[ f.name | f <- every @Fighter db, f.country == "China" ]   -- ["Jet Li"]
(.style) <$> find @Fighter "Rory MacDonald" db              -- Just "Pankration"

-- Joins are do-blocks, checked at compile time
redWinners = Set.fromList $ query db $ do
  b <- match @Bout
  guard (b.winner == b.red)
  w <- follow b.winner
  pure w.name
-- fromList ["Gegard Mousasi","Jet Li"]

-- Results are lists, so aggregations are plain Haskell
standings = sortOn (Down . snd) . Map.toList . Map.fromListWith (+) $ query db $ do
  b <- match @Bout
  w <- follow b.winner
  pure (w.name, 1 :: Int)
-- [("Jet Li",3),("Gegard Mousasi",1)]

-- Recursive rules are recursive functions. closure is their fixed point
beat f = do
  b <- matchBy @Bout @"winner" (refOf f)
  follow (if b.red == b.winner then b.blue else b.red)

beaten d by = [ f.name | f <- query d (closure beat =<< matchBy @Fighter @"name" by) ]

-- Db is a value, share it freely
wins :: Db -> Fighter -> Int
wins d f = length (query d (matchBy @Bout @"winner" (ref @Fighter f.name)))

wins db gegard   -- 1

-- Speculative branches and time travel
Right whatIfDb = commit_ db [Insert (bout "B5" li gegard gegard)]
dbAtTime = asOf (basisT db0) db   -- before anything was written

[ wins d gegard | d <- [db, whatIfDb, dbAtTime] ]       -- [1,2,0]
[ beaten d "Gegard Mousasi" | d <- [db, whatIfDb] ]
-- [["Rory MacDonald"],["Rory MacDonald","Jet Li","Gegard Mousasi"]]

-- A threadsafe Conn holds one shared db that changes
conn <- connect db
Right (_, now) <- transact conn [Upsert gegard { style = "Kickboxing" }]
[ (.style) <$> find @Fighter "Gegard Mousasi" d | d <- [db, now] ]
-- [Just "Mixed",Just "Kickboxing"]

-- Bring your own storage: the log is the db
writeFile "db.log" (unlines (map show (history db)))
restored <- fromHistory . map read . lines <$> readFile' "db.log"
length (every @Bout restored)   -- 4
