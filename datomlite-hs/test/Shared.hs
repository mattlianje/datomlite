{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

-- Records that share field names, and the by-name helpers that cope with them:
-- setField, matchBy, exists and none.
module Shared (sharedTests) where

import           Control.Monad    (guard)
import           Data.Set         (Set)
import qualified Data.Set         as Set
import           Data.Text        (Text)
import           GHC.Generics     (Generic)

import           Test.Tasty
import           Test.Tasty.HUnit

import           Datomlite

data Team = Team { code :: Text, name :: Text }
  deriving (Generic, Show, Eq)
instance Entity Team where
  type Identity Team = '["code"]

data Tag = Tag { name :: Text }
  deriving (Generic, Show, Eq)
instance Entity Tag where
  type Identity Tag = '["name"]

data Member = Member
  { email    :: Text
  , name     :: Text
  , team     :: Ref Team
  , tags     :: Set (Ref Tag)
  , nickname :: Maybe Text
  , level    :: Int
  } deriving (Generic, Show, Eq)
instance Entity Member where
  type Identity Member = '["email"]

data Purchase = Purchase { buyer :: Text, item :: Text }
  deriving (Generic, Show, Eq, Entity)

-- A record stored under a custom prefix.
data Renamed = Renamed { label :: Text }
  deriving (Generic, Show, Eq)
instance Entity Renamed where
  type Identity Renamed = '["label"]
  ename _ = "x.Renamed"

data World = World
  { db :: Db, eng :: Ref Team, sal :: Ref Team, hs :: Ref Tag, max_ :: Ref Member }

world :: World
world = either (error . show) id $ do
  ((e, s, h, m), d) <- commit emptyDb $ do
    e <- insert (Team "ENG" "Engineering")
    s <- insert (Team "SAL" "Sales")
    [h, sc] <- traverse insert [Tag "haskell", Tag "scala"]
    m <- insert (Member "m@x.io" "Max" e (Set.fromList [h, sc]) (Just "mx") 3)
    _ <- insert (Member "a@x.io" "Ann" e (Set.singleton sc)     Nothing     2)
    _ <- insert (Member "s@x.io" "Sam" s mempty                 Nothing     3)
    _ <- insert (Purchase "m@x.io" "book")
    _ <- insert (Purchase "z@x.io" "lamp")
    pure (e, s, h, m)
  pure (World d e s h m)

sharedTests :: TestTree
sharedTests = testGroup "Shared field names (setField, matchBy, exists, none)"
  [ setFieldTests, matchByTests, subqueryTests ]

setFieldTests :: TestTree
setFieldTests = testGroup "setField"
  [ testCase "reads of a shared field name are unambiguous" $ do
      let w = world
      query w.db (do m <- match @Member; t <- follow m.team; pure (m.name, t.name))
        @?= [("Max", "Engineering"), ("Ann", "Engineering"), ("Sam", "Sales")]

  , testCase "sets a field whose name another record shares" $ do
      let w = world
      Right db' <- pure $ commit_ w.db $ do
        update w.max_ (setField @"name" "Maximilian")
        update w.eng  (setField @"name" "Platform")
      (.name) <$> load w.max_ db' @?= Just "Maximilian"
      (.name) <$> load w.eng  db' @?= Just "Platform"

  , testCase "composes, and leaves the other fields alone" $ do
      let w = world
      Right db' <- pure $ commit_ w.db $
        update w.max_ (setField @"level" 4 . setField @"nickname" Nothing)
      let was = load w.max_ w.db
          now = load w.max_ db'
      ((.level) <$> now, (.nickname) <$> now) @?= (Just 4, Just Nothing)
      ((.name) <$> now, (.tags) <$> now) @?= ((.name) <$> was, (.tags) <$> was)

  , testCase "an identity field stays unique" $ do
      let w = world
      Right db' <- pure $ commit_ w.db (update w.max_ (setField @"email" "max@x.io"))
      (.email) <$> load w.max_ db' @?= Just "max@x.io"
      Just ann <- pure (find @Member "a@x.io" w.db)
      commit_ w.db (update w.max_ (setField @"email" "a@x.io"))
        @?= Left (UniqueViolation "Member/email" (VText "a@x.io") (map refId [w.max_, refOf ann]))
  ]

matchByTests :: TestTree
matchByTests = testGroup "matchBy"
  [ testCase "by identity" $ do
      let w = world
      query w.db (do m <- matchBy @Member @"email" "a@x.io"; pure m.name) @?= ["Ann"]
      query w.db (do m <- matchBy @Member @"email" "nobody"; pure m.name) @?= []

  , testCase "by a plain field, in creation order" $ do
      let w = world
      query w.db (do m <- matchBy @Member @"level" 3; pure m.name) @?= ["Max", "Sam"]

  , testCase "by a ref: a reverse lookup" $ do
      let w = world
      query w.db (do m <- matchBy @Member @"team" w.eng; pure m.name) @?= ["Max", "Ann"]
      query w.db (do t <- matchBy @Team @"code" "SAL"
                     m <- matchBy @Member @"team" (refOf t)
                     pure m.name) @?= ["Sam"]

  , testCase "by one element of a Set field" $ do
      let w = world
      query w.db (do m <- matchBy @Member @"tags" w.hs; pure m.name) @?= ["Max"]

  , testCase "by the value inside a Maybe field" $ do
      let w = world
      query w.db (do m <- matchBy @Member @"nickname" "mx"; pure m.name) @?= ["Max"]

  , testCase "agrees with match + guard, and tracks updates and retracts" $ do
      let w = world
      Right db' <- pure $ commit_ w.db $ do
        update w.max_ (setField @"team" w.sal)
        now <- txDb
        [ann] <- pure (query now (refOf <$> matchBy @Member @"name" "Ann"))
        retract ann
      let viaIndex = query db' (do m <- matchBy @Member @"team" w.sal; pure m.name)
          viaScan  = query db' (do m <- match @Member; guard (m.team == w.sal); pure m.name)
      viaIndex @?= ["Max", "Sam"]
      viaIndex @?= viaScan
      query db' (matchBy @Member @"email" "a@x.io") @?= []
      -- and the past is still there
      query (asOf (basisT w.db) db') (do m <- matchBy @Member @"email" "a@x.io"; pure m.name)
        @?= ["Ann"]

  , testCase "respects a custom ename" $ do
      Right (r, d) <- pure $ commit emptyDb (insert (Renamed "x"))
      query d (refOf <$> matchBy @Renamed @"label" "x") @?= [r]
  ]

subqueryTests :: TestTree
subqueryTests = testGroup "exists / none"
  [ testCase "none: an anti-join" $ do
      let w = world
      query w.db (do m <- match @Member
                     none (matchBy @Purchase @"buyer" m.email)
                     pure m.name) @?= ["Ann", "Sam"]

  , testCase "exists: a semi-join, one row however many matches" $ do
      let w = world
      query w.db (do p <- match @Purchase
                     exists (matchBy @Member @"email" p.buyer)
                     pure p.item) @?= ["book"]
      query w.db (do t <- match @Team
                     exists (matchBy @Member @"team" (refOf t))
                     pure t.name) @?= ["Engineering", "Sales"]

  , testCase "subqueries see the same db as the outer query" $ do
      let w = world
      Right db' <- pure $ commit_ w.db (insert (Purchase "a@x.io" "pen"))
      let buyers d = query d $ do
            m <- match @Member
            exists (matchBy @Purchase @"buyer" m.email)
            pure m.name
      (buyers w.db, buyers db', buyers (asOf (basisT w.db) db'))
        @?= (["Max"], ["Max", "Ann"], ["Max"])
  ]
