{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Demo (demoTests) where

import           Control.Applicative    ((<|>))
import           Control.Concurrent     (newEmptyMVar, putMVar, takeMVar)
import           Control.Concurrent.STM (atomically)
import           Control.Monad          (guard, when)
import           Data.List              (sort, sortOn, (\\))
import qualified Data.Map.Strict        as Map
import           Data.Ord               (Down (..))
import           Data.Text              (Text)
import           GHC.Generics           (Generic)
import           System.Timeout         (timeout)

import           Test.Tasty
import           Test.Tasty.HUnit

import           Datomlite

-- Records are the schema, refs are just fields
data Fighter = Fighter { name :: Text, style :: Text, country :: Text }
  deriving (Show, Generic)
instance Entity Fighter where
  type Identity Fighter = '["name"]

data Bout = Bout
  { code   :: Text
  , red    :: Ref Fighter
  , blue   :: Ref Fighter
  , winner :: Ref Fighter
  } deriving (Show, Generic)
instance Entity Bout where
  type Identity Bout = '["code"]

ok :: Show e => Either e a -> a
ok = either (error . show) id

-- A db is a value. One commit, refs point at fighters by name
db :: Db
db = ok $ commit_ emptyDb
  [ Insert (Fighter "Jet Li"         "Wushu"      "China")
  , Insert (Fighter "Gegard Mousasi" "Mixed"      "Netherlands")
  , Insert (Fighter "Rory MacDonald" "Pankration" "Canada")
  , Insert (Bout "B1" li     gegard li)
  , Insert (Bout "B2" li     rory   li)
  , Insert (Bout "B3" gegard rory   gegard)
  , Insert (Bout "B4" rory   li     li)
  ]
  where
    li     = ref @Fighter "Jet Li"
    gegard = ref @Fighter "Gegard Mousasi"
    rory   = ref @Fighter "Rory MacDonald"

-- Who beat whom: a join across both corners of every bout
results :: Query (Text, Text)
results = do
  b <- match @Bout
  w <- follow b.winner
  l <- follow (if b.winner == b.red then b.blue else b.red)
  pure (w.name, l.name)

-- Rollups are plain Haskell over the rows
standings :: Db -> [(Text, Int)]
standings d = sortOn (Down . snd) . Map.toList . Map.fromListWith (+) $ query d $ do
  b <- match @Bout
  w <- follow b.winner
  pure (w.name, 1)

-- A rule is a function returning a Query
beat :: Stored Fighter -> Query (Stored Fighter)
beat f = do
  b <- matchBy @Bout @"winner" (refOf f)
  l <- follow b.red <|> follow b.blue
  guard (refOf l /= refOf f)
  pure l

named :: Text -> Query (Stored Fighter)
named = matchBy @Fighter @"name"

-- A Tx block: steps that can check and bail
book :: Text -> Text -> Text -> Text -> Tx (Ref Bout)
book c r b w = do
  when (r == b) $ abort "can't fight yourself"
  when (w /= r && w /= b) $ abort "winner must be in the bout"
  insert (Bout c (ref @Fighter r) (ref @Fighter b) (ref @Fighter w))

-- Rory gets his revenge, on a branch we may not keep
upset :: Db
upset = ok $ commit_ db (book "B5" "Jet Li" "Rory MacDonald" "Rory MacDonald")

demoTests :: TestTree
demoTests = testGroup "Demo (a fight db tour)"
  [ testCase "reads feel like a collection" $ do
      length (every @Fighter db) @?= 3
      [ f.name | f <- every @Fighter db, f.country == "China" ] @?= ["Jet Li"]
      (.style) <$> find @Fighter "Gegard Mousasi" db @?= Just "Mixed"

  , testCase "joins are do-blocks that follow refs" $
      query db results @?=
        [ ("Jet Li", "Gegard Mousasi")
        , ("Jet Li", "Rory MacDonald")
        , ("Gegard Mousasi", "Rory MacDonald")
        , ("Jet Li", "Rory MacDonald")
        ]

  , testCase "reverse refs are index lookups" $
      query db (do f <- named "Jet Li"
                   b <- matchBy @Bout @"winner" (refOf f)
                   pure b.code)
        @?= ["B1", "B2", "B4"]

  , testCase "aggregations are plain Haskell" $
      standings db @?= [("Jet Li", 3), ("Gegard Mousasi", 1)]

  , testCase "window functions rank with ties" $
      window (const ()) (Down . snd) rank (standings upset) @?=
        [ (("Jet Li", 3), 1)
        , (("Gegard Mousasi", 1), 2)
        , (("Rory MacDonald", 1), 2)
        ]

  , testCase "negation: fighters who never won" $
      query db (do f <- match @Fighter
                   none (matchBy @Bout @"winner" (refOf f))
                   pure f.name)
        @?= ["Rory MacDonald"]

  , testCase "recursive rules: everyone Jet Li beat, transitively" $
      sort (query db (do t <- closure beat =<< named "Jet Li"; pure t.name))
        @?= ["Gegard Mousasi", "Rory MacDonald"]

  , testCase "closure is safe on cycles" $ do
      -- after the upset, Rory > Li > Gegard > Rory
      query db (do t <- closure beat =<< named "Rory MacDonald"; pure t.name) @?= []
      sort (query upset (do t <- closure beat =<< named "Rory MacDonald"; pure t.name))
        @?= ["Gegard Mousasi", "Jet Li", "Rory MacDonald"]

  , testCase "Tx blocks commit all-or-nothing" $ do
      case commit_ db (book "B5" "Jet Li" "Jet Li" "Jet Li") of
        Left (Aborted m) -> m @?= "can't fight yourself"
        other            -> assertFailure (show (fmap basisT other))
      case commit_ db (book "B5" "Jet Li" "Bruce Lee" "Jet Li") of
        Left (UnknownIdentity _ _) -> pure ()
        other                      -> assertFailure (show (fmap basisT other))

  , testCase "what-if branches leave db untouched" $ do
      standings upset @?= [("Jet Li", 3), ("Gegard Mousasi", 1), ("Rory MacDonald", 1)]
      standings db    @?= [("Jet Li", 3), ("Gegard Mousasi", 1)]

  , testCase "upsert keeps refs, time travel and diff see the change" $ do
      let db' = ok $ commit_ db [Upsert (Fighter "Gegard Mousasi" "Kickboxing" "Netherlands")]
          gegardStyle d = query d $ do
            b <- matchBy @Bout @"code" "B3"
            w <- follow b.winner
            pure w.style
      gegardStyle db'                         @?= ["Kickboxing"]
      gegardStyle (asOf (basisT db) db')      @?= ["Mixed"]
      [ dValue d | d <- datoms db' \\ datoms db ] @?= [VText "Kickboxing"]
      [ dValue d | d <- datoms db \\ datoms db' ] @?= [VText "Mixed"]

  , testCase "the log is the database: save and restore" $ do
      let saved = unlines (map show (history upset))
      fromHistory (map read (lines saved)) @?= upset

  , testCase "a Conn shares one changing db, watchers see each commit" $ do
      conn <- connect db
      seen <- newEmptyMVar
      stop <- watch conn standings (putMVar seen)
      first <- timeout 1000000 (takeMVar seen)
      first @?= Just [("Jet Li", 3), ("Gegard Mousasi", 1)]
      Right (_, db') <- transact conn (book "B5" "Jet Li" "Rory MacDonald" "Rory MacDonald")
      next <- timeout 1000000 (takeMVar seen)
      next @?= Just (standings db')
      now <- atomically (awaitAfter conn (basisT db))
      basisT now @?= basisT db'
      stop
  ]
