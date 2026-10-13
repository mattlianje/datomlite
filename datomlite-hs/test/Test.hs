{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot  #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeApplications     #-}

import           Control.Applicative    ((<|>))
import           Control.Concurrent     (forkIO, threadDelay)
import           Control.Concurrent.STM (TVar, atomically, check, modifyTVar',
                                         newTVarIO, readTVar, readTVarIO)
import           Control.Exception      (ErrorCall (..), evaluate, throwIO, try)
import           Control.Monad   (forM_, guard, void, when)
import qualified Data.ByteString as BS
import           Data.List       (sort)
import           Data.Ord        (Down (..))
import           Data.Set        (Set)
import qualified Data.Set        as Set
import           Data.String     (fromString)
import           Data.Text       (Text)
import           Data.Time       (Day, UTCTime (..), fromGregorian)
import           GHC.Generics    (Generic)
import           System.Timeout  (timeout)

import           Test.Tasty
import           Test.Tasty.HUnit hiding (assert)

import           Datomlite
import           Datomlite.Raw

import           Shared           (sharedTests)

data Department = Department
  { code     :: Key Text
  , deptName :: Text
  } deriving (Generic, Show, Eq, Entity)

data Skill = Skill
  { skillName :: Key Text
  } deriving (Generic, Show, Eq, Entity)

data Employee = Employee
  { email  :: Key Text
  , name   :: Text
  , nick   :: Maybe Text
  , dept   :: Ref Department
  , skills :: Set (Ref Skill)
  } deriving (Generic, Show, Eq, Entity)

data Account = Account
  { handle :: Key Text
  , mail   :: Key Text
  } deriving (Generic, Show, Eq, Entity)

data Person = Person
  { pname :: Key Text
  , boss  :: Maybe (Ref Person)
  } deriving (Generic, Show, Eq, Entity)

data Place = Place { place :: Key Text } deriving (Generic, Show, Eq, Entity)
data Road = Road { from :: Ref Place, to :: Ref Place } deriving (Generic, Show, Eq, Entity)

data Pal = Pal { pal :: Key Text } deriving (Generic, Show, Eq, Entity)
data Friend = Friend { fa :: Ref Pal, fb :: Ref Pal } deriving (Generic, Show, Eq, Entity)

data Note = Note
  { body :: Maybe Text
  } deriving (Generic, Show, Eq, Entity)

-- Every type of scalar
data Level = Low | High
  deriving stock (Generic, Show, Eq, Ord)
  deriving anyclass (ToValue, FromValue)

newtype Sensor = Sensor Text
  deriving newtype (Show, Eq, Ord, ToValue, FromValue)

data Reading = Reading
  { sensor  :: Key Sensor
  , level   :: Level
  , celsius :: Double
  , ticks   :: Integer
  , payload :: BS.ByteString
  , takenAt :: UTCTime
  , takenOn :: Day
  , levels  :: Set Level
  } deriving (Generic, Show, Eq, Entity)

reading :: Reading
reading = Reading
  { sensor  = Key (Sensor "s1")
  , level   = High
  , celsius = 21.5
  , ticks   = 2 ^ (80 :: Int) -- past the range of Int
  , payload = BS.pack [0, 255, 10]
  , takenAt = UTCTime (fromGregorian 2026 10 13) 3600.25
  , takenOn = fromGregorian 2026 10 13
  , levels  = Set.fromList [Low, High]
  }

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Datomlite"
  [ coreTests
  , schemaTests
  , loadTests
  , valueTests
  , restoreTests
  , updateTests
  , saveTests
  , txTests
  , timeTests
  , uniqueTests
  , queryTests
  , ioTests
  , listenTests
  , closureTests
  , windowTests
  , sharedTests
  ]

-- Low-level EAV log
coreTests :: TestTree
coreTests = testGroup "Core (EAV log)"
  [ testCase "transact is pure; input db unchanged" $ do
      let e = EntityId 1
      let (_, db1) = transact [assert e "x" (VInt 1)] emptyDb
      history emptyDb @?= []
      length (history db1) @?= 1

  , testCase "asOf hides later transactions" $ do
      let e = EntityId 1
      let (_, db1) = transact [assert e "a" (VText "one")] emptyDb
      let (_, db2) = transact [assert e "b" (VText "two")] db1
      lookupValue e "b" (asOf (TxId 1) db2) @?= Nothing
      lookupValue e "a" (asOf (TxId 1) db2) @?= Just (VText "one")

  , testCase "retraction removes a fact" $ do
      let e = EntityId 1
      let (_, db1) = transact [assert e "a" (VText "one")] emptyDb
      let (_, db2) = transact [retract e "a" (VText "one")] db1
      lookupValue e "a" db2 @?= Nothing

  , testCase "entitiesWith follows asserts and retracts" $ do
      let (a, b) = (EntityId 1, EntityId 2)
      let (_, db1) = transact [assert a "k" (VInt 7), assert b "k" (VInt 7)] emptyDb
      let (_, db2) = transact [retract a "k" (VInt 7)] db1
      entitiesWith "k" (VInt 7) db1 @?= [a, b]
      entitiesWith "k" (VInt 7) db2 @?= [b]
      entitiesWith "k" (VInt 7) (asOf (TxId 1) db2) @?= [a, b]

  , testCase "a fact asserted twice is one fact" $ do
      let e = EntityId 1
      let (_, db1) = transact [assert e "a" (VInt 1), assert e "a" (VInt 1)] emptyDb
      lookupValues e "a" db1 @?= [VInt 1]
      let (_, db2) = transact [retract e "a" (VInt 1)] db1
      lookupValues e "a" db2 @?= []

  , testCase "many values keep the order they were asserted in" $ do
      let e = EntityId 1
      let (_, db1) = transact [assert e "a" (VInt 3), assert e "a" (VInt 1)] emptyDb
      let (_, db2) = transact [assert e "a" (VInt 2)] db1
      lookupValues e "a" db2 @?= [VInt 3, VInt 1, VInt 2]

  , testCase "entitiesInNamespace matches the exact prefix" $ do
      let (_, db) = transact
            [ assert (EntityId 3) "Dept/x"         (VInt 1)
            , assert (EntityId 2) "Department/x"   (VInt 1)
            , assert (EntityId 1) "Department/y"   (VInt 1)
            , assert (EntityId 4) "Departmental/x" (VInt 1) ] emptyDb
      entitiesInNamespace "Department" db @?= [EntityId 1, EntityId 2]
  ]

schemaTests :: TestTree
schemaTests = testGroup "Schema (records are the schema)"
  [ testCase "new then load round-trips a record" $ do
      let d = Department "ENG" "Engineering"
      Right (dRef, db) <- pure $ commit emptyDb (new d)
      load dRef db @?= Just d

  , testCase "bare literals construct Keys (IsString/Num)" $ do
      Department "ENG" "Engineering" @?= Department (Key "ENG") "Engineering"

  , testCase "retractEntity removes every fact of an entity" $ do
      Right (dRef, db1) <- pure $ commit emptyDb (new (Department "ENG" "Engineering"))
      Right db2 <- pure $ commit_ db1 (retractEntity dRef)
      load dRef db2                     @?= Nothing
      length (loadAll @Department db2) @?= 0
      assertBool "history retains the retracted facts" (not (null (history db2)))

  , testCase "attribute names are Type/field prefixed" $ do
      let d = Department "ENG" "Engineering"
      sort (entityFields d) @?= ["Department/code", "Department/deptName"]

  , testCase "Tx do-block: tempid ref used in a later entity" $ do
      let txn = do
            eng <- new (Department "ENG" "Engineering")
            emp <- new (Employee "m@x.io" "Max" (Just "mx") eng mempty)
            pure (eng, emp)
      Right ((engRef, empRef), db) <- pure $ commit emptyDb txn
      (dept <$> load empRef db) @?= Just engRef
      (name <$> load empRef db) @?= Just "Max"

  , testCase "Maybe field: Nothing writes no datom" $ do
      Right (dRef, db1) <- pure $ commit emptyDb (new (Department "ENG" "Engineering"))
      let emp = Employee "a@x.io" "Ann" Nothing dRef mempty
      Right (eRef, db2) <- pure $ commit db1 (new emp)
      load eRef db2 @?= Just emp

  , testCase "cardinality-many round-trips" $ do
      Right (dRef, dbA) <- pure $ commit emptyDb (new (Department "ENG" "Engineering"))
      Right (sRef, dbB) <- pure $ commit dbA (new (Skill "haskell"))
      let emp = Employee "m@x.io" "Max" (Just "mx") dRef (Set.singleton sRef)
      Right (eRef, dbC) <- pure $ commit dbB (new emp)
      (skills <$> load eRef dbC) @?= Just (Set.singleton sRef)

  , testCase "Maybe (Ref t): an optional reference round-trips" $ do
      Right ((ann, bob), db) <- pure $ commit emptyDb $ do
        ann <- new (Person "ann" Nothing)
        bob <- new (Person "bob" (Just ann))
        pure (ann, bob)
      load ann db @?= Just (Person "ann" Nothing)
      load bob db @?= Just (Person "bob" (Just ann))
      query db (do p <- match @Person; b <- follow =<< oneOf p.boss; pure (p.pname, b.pname))
        @?= [("bob", "ann")]

  , testCase "loadAll pairs each record with its Ref" $ do
      Right ([eng, sal], db) <- pure $ commit emptyDb $ traverse new
        [ Department "ENG" "Engineering", Department "SAL" "Sales" ]
      map fst (loadAll @Department db) @?= [eng, sal]
  ]

loadTests :: TestTree
loadTests = testGroup "Reading back (load)"
  [ testCase "a record that writes no datoms leaves nothing to load" $ do
      Right (n, db) <- pure $ commit emptyDb (new (Note Nothing))
      load n db @?= Nothing
      loadAll @Note db @?= []

  , testCase "a Ref only loads facts of its own type" $ do
      Right (eng, db) <- pure $ commit emptyDb (new (Department "ENG" "Engineering"))
      Just asNote <- pure (fromValue (VRef (refId eng)) :: Maybe (Ref Note))
      load asNote db @?= Nothing

  , testCase "a required field added later throws MissingField" $ do
      let (_, db) = transact [assert (EntityId 1) "Department/code" (VText "ENG")] emptyDb
      r <- try (evaluate (length (loadAll @Department db)))
      r @?= Left (MissingField "Department/deptName" (EntityId 1))

  , testCase "a field whose type changed throws BadValue" $ do
      let (_, db) = transact [ assert (EntityId 1) "Department/code"     (VInt 7)
                             , assert (EntityId 1) "Department/deptName" (VText "Eng") ] emptyDb
      r <- try (evaluate (length (loadAll @Department db)))
      r @?= Left (BadValue "Department/code" [VInt 7] (EntityId 1))

  , testCase "a since view skips entities only partly inside it" $ do
      Right (eng, db1) <- pure $ commit emptyDb (new (Department "ENG" "Engineering"))
      Right db2 <- pure $ commit_ db1 $ do
        update eng (\d -> d { deptName = "Platform" })
        new (Department "SAL" "Sales")
      map snd (loadAll @Department (since (basisT db1) db2)) @?= [Department "SAL" "Sales"]
  ]

valueTests :: TestTree
valueTests = testGroup "Value types"
  [ testCase "every scalar kind round-trips" $ do
      Right (r, db) <- pure $ commit emptyDb (new reading)
      load r db @?= Just reading

  , testCase "an enum is stored as its constructor's name, and matches by value" $ do
      Right (r, db) <- pure $ commit emptyDb (new reading)
      lookupValue (refId r) "Reading/level" db @?= Just (VText "High")
      map refOf (query db (matchBy @Reading @"level" High)) @?= [r]
      map refOf (query db (matchBy @Reading @"levels" Low)) @?= [r]
      map refOf (query db (matchBy @Reading @"level" Low))  @?= []

  , testCase "an enum constructor that no longer exists throws BadValue" $ do
      Right (r, db1) <- pure $ commit emptyDb (new reading)
      let (_, db2) = transact [ retract (refId r) "Reading/level" (VText "High")
                              , assert  (refId r) "Reading/level" (VText "Medium") ] db1
      res <- try (evaluate (load r db2))
      res @?= Left (BadValue "Reading/level" [VText "Medium"] (refId r))

  , testCase "a NaN can be stored, found, and replaced" $ do
      let nan = 0 / 0 :: Double
      Right (r, db1) <- pure $ commit emptyDb (new reading { celsius = nan })
      fmap (isNaN . celsius) (load r db1) @?= Just True
      entitiesWith "Reading/celsius" (VDouble nan) db1 @?= [refId r]
      Right db2 <- pure $ commit_ db1 (update r (\x -> x { celsius = 1.5 }))
      fmap celsius (load r db2) @?= Just 1.5
      entitiesWith "Reading/celsius" (VDouble nan) db2 @?= []

  , testCase "a time with a bottom inside fails its commit" $ do
      let bad = reading { takenAt = UTCTime (fromGregorian 2026 10 13) (error "boom") }
      r <- try (evaluate (commit_ emptyDb (new bad)))
      case r of
        Left (ErrorCall msg) -> msg @?= "boom"
        Right _              -> assertFailure "commit returned with a bottom inside"
  ]

restoreTests :: TestTree
restoreTests = testGroup "Restoring (fromHistory)"
  [ testCase "fromHistory . history gives back the same db" $ do
      fromHistory (history fixture) @?= fixture
      Right db <- pure $ commit_ fixture $ do
        Just (m, _) <- lookupRef <$> txDb
        update m (\e -> e { name = "Maximilian" })
      fromHistory (history db) @?= db
      basisT (fromHistory (history db)) @?= basisT db

  , testCase "a datom per line survives show and read" $ do
      Right db <- pure $ commit_ emptyDb (new reading)
      let file = unlines (map show (history db))
      fromHistory (map read (lines file)) @?= db

  , testCase "a restored db keeps its past" $ do
      Right db1 <- pure $ commit_ fixture (retractEntity (fst (head (loadAll @Employee fixture))))
      let restored = fromHistory (history db1)
      loadAll @Employee restored @?= []
      loadAll @Employee (asOf (basisT fixture) restored) @?= loadAll @Employee fixture

  , testCase "new entities after a restore never reuse an id the log mentions" $ do
      -- entity 9 only appears as a ref target
      let (_, db) = transact [ assert (EntityId 1) "Person/pname" (VText "ann")
                             , assert (EntityId 1) "Person/boss"  (VRef (EntityId 9)) ] emptyDb
      Right (bob, db') <- pure $ commit (fromHistory (history db)) (new (Person "bob" Nothing))
      refId bob @?= EntityId 10
      basisT db' @?= TxId 2

  , testCase "datoms out of order are replayed in tx order" $ do
      Right db <- pure $ commit_ fixture (update (fst (head (loadAll @Department fixture)))
                                                 (\d -> d { deptName = "Platform" }))
      loadAll @Department (fromHistory (reverse (history db))) @?= loadAll @Department db
  ]
  where
    lookupRef d = case loadAll @Employee d of
      (x:_) -> Just x
      []    -> Nothing

updateTests :: TestTree
updateTests = testGroup "update (record-update syntax)"
  [ testCase "replaces a cardinality-one field rather than adding to it" $ do
      Right (mx, db1) <- pure $ commit emptyDb $ do
        eng <- new (Department "ENG" "Engineering")
        new (Employee "m@x.io" "Max" Nothing eng mempty)
      Right db2 <- pure $ commit_ db1 (update mx (\e -> e { name = "Maximilian" }))
      (name <$> load mx db2) @?= Just "Maximilian"
      let eid = refId mx
      lookupValues eid "Employee/name" db2 @?= [VText "Maximilian"]
      (name <$> load mx db1) @?= Just "Max"

  , testCase "Maybe and Set fields: values come and go" $ do
      Right ((mx, hs), db1) <- pure $ commit emptyDb $ do
        eng <- new (Department "ENG" "Engineering")
        hs  <- new (Skill "haskell")
        mx  <- new (Employee "m@x.io" "Max" (Just "mx") eng (Set.singleton hs))
        pure (mx, hs)
      Right (sc, db2) <- pure $ commit db1 (new (Skill "scala"))
      Right db3 <- pure $ commit_ db2 $
        update mx (\e -> e { nick = Nothing, skills = Set.singleton sc })
      (nick   <$> load mx db3) @?= Just Nothing
      (skills <$> load mx db3) @?= Just (Set.singleton sc)
      assertBool "haskell skill no longer listed" (hs `notElem` maybe mempty skills (load mx db3))

  , testCase "sees an entity created earlier in the same Tx" $ do
      Right (eng, db) <- pure $ commit emptyDb $ do
        eng <- new (Department "ENG" "Engineering")
        update eng (\d -> d { deptName = "Platform" })
        pure eng
      (deptName <$> load eng db) @?= Just "Platform"

  , testCase "changing a Key to one already taken is rejected" $ do
      Right ([_, sal], db) <- pure $ commit emptyDb $ traverse new
        [ Department "ENG" "Engineering", Department "SAL" "Sales" ]
      commit_ db (update sal (\d -> d { code = "ENG" }))
        @?= Left (UniqueViolation "Department/code" (VText "ENG"))

  , testCase "updating a retracted entity fails" $ do
      Right (eng, db1) <- pure $ commit emptyDb (new (Department "ENG" "Engineering"))
      Right db2 <- pure $ commit_ db1 (retractEntity eng)
      let eid = refId eng
      commit_ db2 (update eng id) @?= Left (UnknownEntity eid)
  ]

saveTests :: TestTree
saveTests = testGroup "save (upsert by Key)"
  [ testCase "inserts when no entity holds the key" $ do
      Right (eng, db) <- pure $ commit emptyDb (save (Department "ENG" "Engineering"))
      load eng db @?= Just (Department "ENG" "Engineering")

  , testCase "updates the entity holding the key; refs to it survive" $ do
      Right ((eng, mx), db1) <- pure $ commit emptyDb $ do
        eng <- new (Department "ENG" "Engineering")
        mx  <- new (Employee "m@x.io" "Max" Nothing eng mempty)
        pure (eng, mx)
      Right (eng', db2) <- pure $ commit db1 (save (Department "ENG" "Platform"))
      eng' @?= eng
      (deptName <$> load eng db2) @?= Just "Platform"
      length (loadAll @Department db2) @?= 1
      let eid = refId eng
      lookupValues eid "Department/deptName" db2 @?= [VText "Platform"]
      query db2 (do e <- follow mx; d <- follow e.dept; pure d.deptName) @?= ["Platform"]

  , testCase "saving an unchanged record writes nothing" $ do
      Right (_, db1) <- pure $ commit emptyDb (new (Department "ENG" "Engineering"))
      Right db2 <- pure $ commit_ db1 (save (Department "ENG" "Engineering"))
      history db2 @?= history db1

  , testCase "sees an entity created earlier in the same Tx" $ do
      Right ((a, b), db) <- pure $ commit emptyDb $ do
        a <- save (Department "ENG" "Engineering")
        b <- save (Department "ENG" "Platform")
        pure (a, b)
      a @?= b
      map snd (loadAll db) @?= [Department "ENG" "Platform"]

  , testCase "keys held by two different entities are rejected" $ do
      Right (_, db1) <- pure $ commit emptyDb $ traverse new
        [ Account "max" "m@x.io", Account "sam" "s@x.io" ]
      commit_ db1 (save (Account "max" "s@x.io"))
        @?= Left (UniqueViolation "Account/mail" (VText "s@x.io"))

  , testCase "works over a Conn" $ do
      conn <- connect emptyDb
      Right eng  <- commitIO conn (save (Department "ENG" "Engineering"))
      Right eng' <- commitIO conn (save (Department "ENG" "Platform"))
      db <- dbOf conn
      eng' @?= eng
      map snd (loadAll db) @?= [Department "ENG" "Platform"]
  ]

txTests :: TestTree
txTests = testGroup "Tx (txDb, abort)"
  [ testCase "txDb sees the transaction's own writes" $ do
      Right ((n0, n1), db) <- pure $ commit emptyDb $ do
        b <- length . loadAll @Department <$> txDb
        _ <- new (Department "ENG" "Engineering")
        a <- length . loadAll @Department <$> txDb
        pure (b, a)
      (n0, n1) @?= (0, 1)
      length (loadAll @Department db) @?= 1

  , testCase "abort fails the commit and writes nothing" $ do
      let txn = do
            _ <- new (Department "ENG" "Engineering")
            abort "changed my mind" :: Tx ()
      commit_ emptyDb txn @?= Left (Aborted "changed my mind")

  , testCase "read-modify-write: a failed pattern aborts" $ do
      Right (eng, db1) <- pure $ commit emptyDb (new (Department "ENG" "Engineering"))
      Right db2 <- pure $ commit_ db1 (retractEntity eng)
      let rename = do
            Just d <- load eng <$> txDb
            update eng (const d { deptName = d.deptName <> " (renamed)" })
      Right db3 <- pure $ commit_ db1 rename
      (deptName <$> load eng db3) @?= Just "Engineering (renamed)"
      case commit_ db2 rename of
        Left (Aborted _) -> pure ()
        other            -> assertFailure (show other)
  ]

timeTests :: TestTree
timeTests = testGroup "Time travel (basisT, asOf)"
  [ testCase "each commit advances basisT" $ do
      Right (_, db1) <- pure $ commit emptyDb (new (Department "ENG" "Engineering"))
      Right db2      <- pure $ commit_ db1 (pure ())
      assertBool "basisT increases" (basisT emptyDb < basisT db1 && basisT db1 < basisT db2)

  , testCase "asOf (basisT db) brings back that value" $ do
      Right (eng, db1) <- pure $ commit emptyDb (new (Department "ENG" "Engineering"))
      let t = basisT db1
      Right db2 <- pure $ commit_ db1 (update eng (\d -> d { deptName = "Platform" }))
      (deptName <$> load eng (asOf t db2)) @?= Just "Engineering"
      (deptName <$> load eng db2)          @?= Just "Platform"
      basisT (asOf t db2) @?= t

  , testCase "commitAt stamps the tx and asOfTime rewinds to it" $ do
      Right (eng, db1) <- pure $ commitAt (at 9) emptyDb (new (Department "ENG" "Engineering"))
      Right (_, db2) <- pure $ commitAt (at 12) db1 (update eng (\d -> d { deptName = "Platform" }))
      txTime (basisT db1) db2 @?= Just (at 9)
      txTime (basisT db2) db2 @?= Just (at 12)
      (deptName <$> load eng (asOfTime (at 10) db2)) @?= Just "Engineering"
      (deptName <$> load eng (asOfTime (at 12) db2)) @?= Just "Platform"
      load eng (asOfTime (at 8) db2) @?= Nothing

  , testCase "an unstamped commit goes with the stamp before it" $ do
      Right (_, db1) <- pure $ commitAt (at 9) emptyDb (new (Skill "one"))
      Right db2 <- pure $ commit_ db1 (new (Skill "two"))
      Right (_, db3) <- pure $ commitAt (at 12) db2 (new (Skill "three"))
      txTime (basisT db2) db3 @?= Nothing
      map (skillName . snd) (loadAll (asOfTime (at 10) db3)) @?= ["one", "two"]

  , testCase "tx times never go backwards" $ do
      Right (_, db1) <- pure $ commitAt (at 12) emptyDb (pure ())
      Right (_, db2) <- pure $ commitAt (at 9) db1 (pure ())
      txTime (basisT db2) db2 @?= Just (at 12)

  , testCase "tx times survive a save and restore" $ do
      Right (_, db1) <- pure $ commitAt (at 9) emptyDb (new (Skill "one"))
      let db' = fromHistory (map (read . show) (history db1))
      txTime (basisT db1) db' @?= Just (at 9)
      datoms db' @?= datoms db1
  ]
  where
    at h = UTCTime (fromGregorian 2026 10 13) (h * 3600)

uniqueTests :: TestTree
uniqueTests = testGroup "Uniqueness (Key)"
  [ testCase "duplicate key vs existing db is rejected" $ do
      Right (_, db1) <- pure $ commit emptyDb (new (Department "ENG" "Engineering"))
      commit db1 (new (Department "ENG" "Engineering (dup)"))
        @?= Left (UniqueViolation "Department/code" (VText "ENG"))

  , testCase "distinct keys are fine" $ do
      case commit emptyDb $ traverse new [ Department "ENG" "Engineering"
                                         , Department "SAL" "Sales" ] of
        Right (refs, _) -> length refs @?= 2
        Left err        -> assertFailure (show err)

  , testCase "duplicate keys within one transaction are rejected" $ do
      let txn = do
            _ <- new (Department "ENG" "Engineering")
            new (Department "ENG" "Engineering again")
      commit_ emptyDb txn @?= Left (UniqueViolation "Department/code" (VText "ENG"))

  , testCase "failed transaction writes nothing" $ do
      Right (_, db1) <- pure $ commit emptyDb (new (Department "ENG" "Engineering"))
      let bad = do
            _ <- new (Skill "haskell")
            new (Department "ENG" "dup")
      commit_ db1 bad @?= Left (UniqueViolation "Department/code" (VText "ENG"))
      length (loadAll @Skill db1) @?= 0

  , testCase "a key can be retracted and reused in one transaction" $ do
      Right (old, db1) <- pure $ commit emptyDb (new (Department "ENG" "Engineering"))
      Right (new', db2) <- pure $ commit db1 $ do
        retractEntity old
        new (Department "ENG" "Engineering, reborn")
      (deptName <$> load new' db2) @?= Just "Engineering, reborn"
      load old db2 @?= Nothing
  ]

queryTests :: TestTree
queryTests = testGroup "Query (do-blocks over records)"
  [ testCase "join by following a ref" $ do
      let result = query fixture $ do
            e <- match @Employee
            d <- follow e.dept
            guard (d.deptName == "Engineering")
            pure (e.email, d.deptName)
      result @?= [("m@x.io", "Engineering")]

  , testCase "join with match + guard against ref" $ do
      let result = query fixture $ do
            d <- match @Department
            e <- match @Employee
            guard (e.dept == refOf d)
            pure (d.code, e.name)
      result @?= [("ENG", "Max")]

  , testCase "oneOf ranges over a cardinality-many field" $ do
      let result = query fixture $ do
            e <- match @Employee
            s <- follow =<< oneOf e.skills
            pure (e.email, s.skillName)
      result @?= [("m@x.io", "haskell")]

  , testCase "literal filter narrows results" $ do
      let result = query fixture $ do
            e <- match @Employee
            guard (e.name == "Max" && e.nick == Just "mx")
            pure e.email
      result @?= ["m@x.io"]

  , testCase "absent Maybe drops the row (oneOf, or a failed pattern)" $ do
      Right (dRef, db1) <- pure $ commit emptyDb (new (Department "ENG" "Engineering"))
      Right (_,    db2) <- pure $ commit db1 $ traverse new
        [ Employee "m@x.io" "Max" (Just "mx") dRef mempty
        , Employee "a@x.io" "Ann" Nothing     dRef mempty ]
      query db2 (do e <- match @Employee; oneOf e.nick) @?= ["mx"]
      query db2 (do e <- match @Employee; Just n <- pure e.nick; pure n) @?= ["mx"]

  , testCase "compare a ref field to a Ref held outside the query" $ do
      Right ((eng, sal), db) <- pure $ commit emptyDb $ do
        eng <- new (Department "ENG" "Engineering")
        sal <- new (Department "SAL" "Sales")
        _   <- new (Employee "m@x.io" "Max" Nothing eng mempty)
        _   <- new (Employee "s@x.io" "Sam" Nothing sal mempty)
        pure (eng, sal)
      let inDept r = query db $ do
            e <- match @Employee
            guard (e.dept == r)
            pure e.name
      (inDept eng, inDept sal) @?= (["Max"], ["Sam"])

  , testCase "return whole records and refs" $ do
      let result = query fixture $ do
            d <- match @Department
            pure (refOf d, recordOf d)
      map snd result @?= [Department "ENG" "Engineering"]
      map fst result @?= map fst (loadAll @Department fixture)

  , testCase "querying an older db value time-travels" $ do
      [(mx, _)] <- pure (loadAll @Employee fixture)
      Right db2 <- pure $ commit_ fixture (update mx (\e -> e { name = "Maximilian" }))
      let names db = query db (do e <- match @Employee; pure e.name)
      (names fixture, names db2) @?= (["Max"], ["Maximilian"])
  ]

ioTests :: TestTree
ioTests = testGroup "IO edge (Conn + commitIO)"
  [ testCase "commitIO commits a Tx and returns its refs" $ do
      conn <- connect emptyDb
      res  <- commitIO conn $ do
        eng <- new (Department "ENG" "Engineering")
        new (Employee "m@x.io" "Max" Nothing eng mempty)
      case res of
        Left err     -> assertFailure (show err)
        Right empRef -> do
          db <- dbOf conn
          (name <$> load empRef db) @?= Just "Max"

  , testCase "commitIO enforces Key uniqueness and writes nothing on clash" $ do
      conn <- connect emptyDb
      _    <- commitIO conn (new (Department "ENG" "Engineering"))
      bad  <- commitIO conn (new (Department "ENG" "dup"))
      bad @?= Left (UniqueViolation "Department/code" (VText "ENG"))
      db <- dbOf conn
      length (loadAll @Department db) @?= 1

  , testCase "commit is strict: a bottom value fails the commit, not a later read" $ do
      r <- try (evaluate (commit_ emptyDb (new (Department "ENG" (error "boom")))))
      case r of
        Left (ErrorCall msg) -> msg @?= "boom"
        Right _              -> assertFailure "commit returned with a bottom inside"

  , testCase "a commitIO that throws leaves the Conn untouched" $ do
      conn <- connect emptyDb
      bad  <- try (commitIO conn (new (Department "ENG" (error "boom"))))
      either (\(ErrorCall msg) -> msg @?= "boom") (const (assertFailure "no throw")) bad
      bad' <- try (commitIO conn (txDb >> error "boom" :: Tx ()))
      either (\(ErrorCall msg) -> msg @?= "boom") (const (assertFailure "no throw")) bad'
      ok <- commitIO conn (new (Department "SAL" "Sales"))
      void (either (assertFailure . show) pure ok)
      db <- dbOf conn
      basisT db @?= TxId 1
      map (deptName . snd) (loadAll @Department db) @?= ["Sales"]
  ]

listenTests :: TestTree
listenTests = testGroup "Listen and watch"
  [ testCase "listen sees each commit from now on, in tx order, with its datoms" $ do
      conn <- connect emptyDb
      _    <- commitIO conn (new (Skill "before"))
      (seen, rec) <- recorder
      _ <- listen conn rec
      _ <- commitIO conn (new (Department "ENG" "Engineering"))
      _ <- commitIO conn (pure ())
      rs <- waitFor seen ((== 2) . length)
      map trTxId rs @?= [TxId 2, TxId 3]
      map (sort . map dAttr . trDatoms) rs
        @?= [["Department/code", "Department/deptName", "db/txInstant"], ["db/txInstant"]]
      assertBool "stamped" (all (\r -> trTime r /= Nothing) rs)

  , testCase "concurrent commits reach a listener once each, in tx order" $ do
      conn <- connect emptyDb
      (seen, rec) <- recorder
      _ <- listen conn rec
      forM_ [1 .. 50 :: Int] $ \i -> forkIO $ void $ commitIO conn (new (Skill (fromString (show i))))
      rs <- waitFor seen ((== 50) . length)
      map trTxId rs @?= map TxId [1 .. 50]

  , testCase "unsubscribing stops delivery" $ do
      conn <- connect emptyDb
      (seen, rec) <- recorder
      stop <- listen conn rec
      _ <- commitIO conn (new (Skill "one"))
      _ <- waitFor seen ((== 1) . length)
      stop >> stop
      _ <- commitIO conn (new (Skill "two"))
      threadDelay 50000
      length <$> readTVarIO seen >>= (@?= 1)

  , testCase "a listener that throws never fails the commit, and keeps listening" $ do
      conn <- connect emptyDb
      (seen, rec) <- recorder
      _ <- listen conn $ \r -> do
        rec r
        when (trTxId r == TxId 1) (throwIO (userError "expected in test"))
      first <- commitIO conn (new (Skill "one"))
      assertBool "commit succeeded" (either (const False) (const True) first)
      _ <- commitIO conn (new (Skill "two"))
      map trTxId <$> waitFor seen ((== 2) . length) >>= (@?= [TxId 1, TxId 2])

  , testCase "a listener can commit" $ do
      conn <- connect emptyDb
      (seen, rec) <- recorder
      _ <- listen conn $ \r -> do
        rec r
        when (trTxId r == TxId 1) (void (commitIO conn (new (Skill "audit"))))
      _ <- commitIO conn (new (Department "ENG" "Engineering"))
      _ <- waitFor seen ((== 2) . length)
      db <- dbOf conn
      map (skillName . snd) (loadAll db) @?= ["audit"]

  , testCase "watch fires up front, then only when the result changes" $ do
      conn <- connect emptyDb
      (seen, rec) <- recorder
      _ <- watch conn (length . loadAll @Department) rec
      _ <- waitFor seen (== [0])
      _ <- commitIO conn (new (Skill "haskell"))
      _ <- commitIO conn (new (Department "ENG" "Engineering"))
      _ <- waitFor seen ((== Just 1) . lastMay)
      Right eng <- commitIO conn (save (Department "ENG" "Platform"))
      _ <- commitIO conn (retractEntity eng)
      waitFor seen ((== Just 0) . lastMay) >>= (@?= [0, 1, 0])

  , testCase "awaitAfter blocks until a newer commit" $ do
      conn <- connect emptyDb
      t    <- basisT <$> dbOf conn
      _    <- forkIO (threadDelay 10000 >> void (commitIO conn (new (Skill "late"))))
      db   <- atomically (awaitAfter conn t)
      map (skillName . snd) (loadAll db) @?= ["late"]
  ]
  where
    lastMay xs = if null xs then Nothing else Just (last xs)

closureTests :: TestTree
closureTests = testGroup "closure (recursion on cycles)"
  [ testCase "roads: a directed graph with a cycle" $ do
      let next p = do
            r <- matchBy @Road @"from" (refOf p)
            follow r.to
          from' c = sort $ query roads $ do
            t <- closure next =<< matchBy @Place @"place" c
            pure t.place
      from' "PAR" @?= ["BOR", "LYO", "MAR", "NCE", "PAR"]
      from' "NCE" @?= []
      from' "MAR" @?= ["BOR", "LYO", "MAR", "NCE", "PAR"]

  , testCase "friends: an undirected graph, two components" $ do
      let knows x = (matchBy @Friend @"fa" (refOf x) >>= \f -> follow f.fb)
                <|> (matchBy @Friend @"fb" (refOf x) >>= \f -> follow f.fa)
          cluster h = sort $ query friends $ do
            y <- closure knows =<< matchBy @Pal @"pal" h
            pure y.pal
      cluster "alice" @?= ["alice", "bob", "carol", "dave", "erin"]
      cluster "frank" @?= ["frank", "grace"]
      null (query friends $ do
        a <- matchBy @Pal @"pal" "alice"
        y <- closure knows a
        guard (y.pal == "frank")) @?= True

  , testCase "a 2000 place ring terminates" $ do
      let ring = either (error . show) id $ commit_ emptyDb $ do
            ps <- traverse (new . Place . fromString . show) [1 .. 2000 :: Int]
            mapM_ (\(a, b) -> new (Road a b)) (zip ps (drop 1 ps ++ ps))
          next p = matchBy @Road @"from" (refOf p) >>= \r -> follow r.to
      length (query ring (matchBy @Place @"place" "1" >>= \p -> exists (closure next p))) @?= 1
  ]
  where
    roads = either (error . show) id $ commit_ emptyDb $ do
      [par, lyo, mar, nce, bor] <- traverse (new . Place) ["PAR", "LYO", "MAR", "NCE", "BOR"]
      mapM_ (new . uncurry Road) [(par, lyo), (lyo, mar), (mar, par), (lyo, nce), (par, bor)]
    friends = either (error . show) id $ commit_ emptyDb $ do
      [alice, bob, carol, dave, erin, frank, grace] <-
        traverse (new . Pal) ["alice", "bob", "carol", "dave", "erin", "frank", "grace"]
      mapM_ (new . uncurry Friend)
        [(alice, bob), (alice, carol), (bob, dave), (carol, dave), (dave, erin), (frank, grace)]

windowTests :: TestTree
windowTests = testGroup "Window functions"
  [ testCase "rowNumber, rank and denseRank per group" $ do
      let w f = [ (n, x) | ((_, n, _), x) <- window team (Down . pts) f scores ]
      w rowNumber @?= [("a",1),("b",2),("c",3),("d",4),("e",1),("f",2)]
      w rank      @?= [("a",1),("b",2),("c",2),("d",4),("e",1),("f",2)]
      w denseRank @?= [("a",1),("b",2),("c",2),("d",3),("e",1),("f",2)]

  , testCase "lag and lead stop at the group's edge" $ do
      let w f = map snd (window team (Down . pts) f scores)
      w (lag 1 pts)  @?= [Nothing, Just 9, Just 7, Just 7, Nothing, Just 5]
      w (lead 1 pts) @?= [Just 7, Just 7, Just 3, Nothing, Just 2, Nothing]
      w (lag 5 pts)  @?= replicate 6 Nothing

  , testCase "whole and running, several at once" $ do
      let total = sum . map pts
      map snd (window team (Down . pts) ((,) <$> whole total <*> running total) scores)
        @?= [(26,9),(26,16),(26,23),(26,26),(7,5),(7,7)]

  , testCase "const () is one group, ties keep their order" $
      map (\((_, n, _), r) -> (n, r)) (window (const ()) pts rowNumber scores)
        @?= [("f",1),("d",2),("e",3),("b",4),("c",5),("a",6)]
  ]
  where
    scores = [ ("x", "b", 7), ("y", "e", 5), ("x", "a", 9), ("x", "c", 7)
             , ("x", "d", 3), ("y", "f", 2) ] :: [(Text, Text, Int)]
    team (t, _, _) = t
    pts (_, _, p) = p

recorder :: IO (TVar [a], a -> IO ())
recorder = do
  seen <- newTVarIO []
  pure (seen, \x -> atomically (modifyTVar' seen (++ [x])))

waitFor :: Show a => TVar [a] -> ([a] -> Bool) -> IO [a]
waitFor v ok = do
  r <- timeout 2000000 $ atomically $ do
    xs <- readTVar v
    check (ok xs)
    pure xs
  case r of
    Just xs -> pure xs
    Nothing -> readTVarIO v >>= \xs -> assertFailure ("timed out; saw " <> show xs)

fixture :: Db
fixture = case commit_ emptyDb build of
    Right db -> db
    Left err -> error (show err)
  where
    build = do
      eng <- new (Department "ENG" "Engineering")
      hs  <- new (Skill "haskell")
      _   <- new (Employee "m@x.io" "Max" (Just "mx") eng (Set.singleton hs))
      pure ()
