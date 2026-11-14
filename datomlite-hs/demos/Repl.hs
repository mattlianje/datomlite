{-# LANGUAGE DataKinds, DeriveGeneric, DuplicateRecordFields, OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings, TypeApplications, TypeFamilies #-}

import Control.Monad (guard)
import Data.Text (Text)
import GHC.Generics (Generic)
import Datomlite

-- Records are the schema
data Fighter = Fighter { name, style, country :: Text }                   deriving (Show, Generic)
data Bout    = Bout    { code :: Text, red, blue, winner :: Ref Fighter } deriving (Show, Generic)

instance Entity Fighter where type Identity Fighter = '["name"]   -- the identity field
instance Entity Bout    where type Identity Bout    = '["code"]

li     = Fighter "Jet Li"         "Wushu"      "China"
gegard = Fighter "Gegard Mousasi" "Mixed"      "Netherlands"
rory   = Fighter "Rory MacDonald" "Pankration" "Canada"

-- A ref names a fighter by identity
bout :: Text -> Fighter -> Fighter -> Fighter -> Bout
bout code red blue winner = Bout code (to red) (to blue) (to winner)
  where to f = ref @Fighter f.name

-- Your db is a plain value...
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
redWinners = query db $ do
  b <- match @Bout
  guard (b.winner == b.red)
  w <- follow b.winner
  pure (b.code, w.name) -- [("B1","Jet Li"),("B2","Jet Li"),("B3","Gegard Mousasi")]

-- Recursive rules are recursive functions...
beat f = do
  b <- matchBy @Bout @"winner" (refOf f)
  follow (if b.red == b.winner then b.blue else b.red)

beaten d by = map (.name) $ query d $
  closure beat =<< matchBy @Fighter @"name" by

-- Speculative branches and time travel
Right whatIfDb = commit_ db [Insert (bout "B5" li gegard gegard)]
dbAtTime = asOf (basisT db0) db  -- before anything was written

[ beaten d "Gegard Mousasi" | d <- [db, whatIfDb, dbAtTime] ]
-- [["Rory MacDonald"],["Rory MacDonald","Jet Li","Gegard Mousasi"],[]]

-- A threadsafe Conn holds one shared db that changes
conn <- connect db
Right (_, now) <- transact conn [Upsert gegard { style = "Kickboxing" }]

[ (.style) <$> find @Fighter "Gegard Mousasi" d | d <- [db, now] ] -- [Just "Mixed", Just "Kickboxing"]

-- Bring your own storage: the log is the db
writeFile "db.log" (unlines (map show (history db)))
restored <- fromHistory . map read . lines <$> readFile "db.log"

length (every @Bout restored)  -- 4
