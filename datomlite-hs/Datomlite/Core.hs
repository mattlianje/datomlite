{-# LANGUAGE OverloadedStrings #-}

-- The append-only EAV log and the 'Db' value. Two indexes over the live facts
-- move in lockstep with the log. EAVT is for reading an entity and AVET is for
-- keys and lookups by value
module Datomlite.Core
  ( -- * Core EAV values
    EntityId (..)
  , Attr
  , Value (..)
  , TxId (..)
  , Op (..)
  , Datom (..)

    -- * The database
  , Db
  , emptyDb
  , basisT
  , freshEntity
  , partialView
  , txTime

    -- * Low-level transactions
  , TxData
  , assert
  , retract
  , TxReport (..)
  , transact
  , stage
  , closeTx
  , stampTime
  , uniqueAt

    -- * Time travel (pure)
  , asOf
  , asOfTime
  , since
  , history
  , fromHistory

    -- * Reads
  , datoms
  , entityAttrs
  , lookupValue
  , lookupValues
  , entitiesWith
  , entitiesInNamespace

    -- * The IO edge
  , Conn
  , connect
  , snapshot
  , atomicUpdate
  , awaitAfter
  , txsAfter
  ) where

import           Control.Concurrent.MVar (MVar, newMVar, withMVar)
import           Control.Concurrent.STM (STM, TVar, atomically, check, newTVarIO,
                                         readTVar, readTVarIO, writeTVar)
import           Control.Exception (evaluate)
import           Data.ByteString (ByteString)
import           Data.List (foldl', sortOn)
import qualified Data.List.NonEmpty as NE
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (Day, UTCTime)

-- | An entity id. Stable and opaque.
newtype EntityId = EntityId Int
  deriving (Eq, Ord, Show, Read)

-- | An attribute name, e.g. @"Employee/email"@.
type Attr = Text

-- | What a 'Datom' can hold. Strict so a value that throws does so on commit
-- and not on some later read.
--
-- Values key the AVET index so they need an order. @NaN@ equals @NaN@ and
-- sorts above every other double... so it can be stored and retracted like
-- any value.
data Value
  = VInt     !Int
  | VText    !Text
  | VBool    !Bool
  | VRef     !EntityId
  | VDouble  !Double
  | VInteger !Integer
  | VBytes   !ByteString
  | VInstant !UTCTime
  | VDay     !Day
    -- | The entity holding a value for an identity attribute. Only in writes:
    -- 'Datomlite.ref' makes one and commit swaps in the 'VRef' it names, so
    -- it's never stored.
  | VLookup  !Attr !Value
  deriving (Show, Read)

instance Eq Value where
  x == y = compare x y == EQ

instance Ord Value where
  compare (VInt a)     (VInt b)     = compare a b
  compare (VText a)    (VText b)    = compare a b
  compare (VBool a)    (VBool b)    = compare a b
  compare (VRef a)     (VRef b)     = compare a b
  compare (VDouble a)  (VDouble b)  = compareDouble a b
  compare (VInteger a) (VInteger b) = compare a b
  compare (VBytes a)   (VBytes b)   = compare a b
  compare (VInstant a) (VInstant b) = compare a b
  compare (VDay a)     (VDay b)     = compare a b
  compare (VLookup a v) (VLookup b w) = compare (a, v) (b, w)
  compare a b = compare (tag a) (tag b)
    where
      tag :: Value -> Int
      tag v = case v of
        VInt _ -> 0; VText _ -> 1; VBool _ -> 2; VRef _ -> 3; VDouble _ -> 4
        VInteger _ -> 5; VBytes _ -> 6; VInstant _ -> 7; VDay _ -> 8
        VLookup _ _ -> 9

-- A total order on doubles: NaN equals NaN and sorts last
compareDouble :: Double -> Double -> Ordering
compareDouble a b
  | isNaN a   = if isNaN b then EQ else GT
  | isNaN b   = LT
  | otherwise = compare a b

-- | A transaction id. Always increasing and what 'asOf' and 'since' take.
newtype TxId = TxId Int
  deriving (Eq, Ord, Show, Read)

-- | Whether a datom asserts or retracts a fact.
data Op = Assert | Retract
  deriving (Eq, Ord, Show, Read)

-- | One fact. 'Show' and 'Read' round-trip so one datom per line is already a
-- file format (see 'fromHistory').
data Datom = Datom
  { dEntity :: !EntityId
  , dAttr   :: !Attr
  , dValue  :: !Value
  , dTx     :: !TxId
  , dOp     :: !Op
  } deriving (Eq, Show, Read)

-- | The log plus the live indexes built from it and the id counters. Abstract
-- so the indexes can't drift from the log.
--
-- Strict: a 'Db' in WHNF has every datom indexed... so every 'Value' in it is
-- evaluated.
data Db = Db
  { dbLog    :: ![Datom] -- newest first
  , dbEavt   :: !(Map EntityId (Map Attr [Datom])) -- live, oldest first
  , dbAvet   :: !(Map Attr (Map Value (Set EntityId))) -- live
  , dbBasis  :: !TxId -- last tx seen
  , dbNextTx :: !Int
  , dbNextE  :: !Int
  , dbPartial :: !Bool -- a 'since' window
  , dbTimes  :: !(Map TxId UTCTime) -- from the db/txInstant datoms
  } deriving (Eq, Show)

-- | The empty database.
emptyDb :: Db
emptyDb = Db { dbLog = [], dbEavt = Map.empty, dbAvet = Map.empty
             , dbBasis = TxId 0, dbNextTx = 1, dbNextE = 1, dbPartial = False
             , dbTimes = Map.empty }

-- | Whether this is a 'since' view. Those can hold just part of an entity.
partialView :: Db -> Bool
partialView = dbPartial

-- | The last transaction in this 'Db'. Hold on to it to come back with 'asOf':
--
-- @
-- let t = basisT db   -- remember now
-- ...
-- query (asOf t db') q  -- ask then
-- @
basisT :: Db -> TxId
basisT = dbBasis

-- | Hand out a new entity id.
freshEntity :: Db -> (EntityId, Db)
freshEntity db = (EntityId (dbNextE db), db { dbNextE = dbNextE db + 1 })

-- | When a transaction was committed, if it was stamped (see
-- 'Datomlite.Schema.commitAt').
txTime :: TxId -> Db -> Maybe UTCTime
txTime tx db = Map.lookup tx (dbTimes db)

-- The attribute a tx's wall-clock time is logged under
txInstant :: Attr
txInstant = "db/txInstant"

-- Fold one datom into the live indexes. Asserting a live fact again or
-- retracting a dead one is a no-op. A tx time goes to 'dbTimes' and not the
-- indexes... it's about the tx and not an entity
index :: Db -> Datom -> Db
index db d
  | dAttr d == txInstant, VInstant t <- dValue d =
      db { dbTimes = Map.insert (dTx d) t (dbTimes db) }
  | otherwise = case dOp d of
  Assert
    | live      -> db
    | otherwise -> db { dbEavt = Map.insertWith (Map.unionWith (flip (++)))
                                   e (Map.singleton a [d]) (dbEavt db)
                      , dbAvet = Map.insertWith (Map.unionWith Set.union)
                                   a (Map.singleton v (Set.singleton e)) (dbAvet db)
                      }
  Retract
    | live      -> db { dbEavt = Map.update (nonEmpty . Map.update dropV a) e (dbEavt db)
                      , dbAvet = Map.update (nonEmpty . Map.update dropE v) a (dbAvet db)
                      }
    | otherwise -> db
  where
    e = dEntity d; a = dAttr d; v = dValue d
    live = e `Set.member` entitiesWithSet a v db
    dropV ds = nonEmptyList (filter ((/= v) . dValue) ds)
    dropE es = let es' = Set.delete e es in if Set.null es' then Nothing else Just es'
    nonEmpty m = if Map.null m then Nothing else Just m
    nonEmptyList xs = if null xs then Nothing else Just xs

-- Rebuild a database from a log (newest first). The id counters are kept
reindex :: [Datom] -> Db -> Db
reindex lg db =
  foldl' index db { dbLog = lg, dbEavt = Map.empty, dbAvet = Map.empty, dbTimes = Map.empty }
    (reverse lg)

-- | Raw writes: datoms still missing their tx id. Commit stamps it on. Build
-- them with 'assert' and 'retract':
-- @transact [assert e "Person\/name" (VText "Max")] db@.
--
-- For records, use 'Datomlite.Schema.Tx'.
type TxData = [(EntityId, Attr, Value, Op)]

-- | Assert a fact.
assert :: EntityId -> Attr -> Value -> (EntityId, Attr, Value, Op)
assert e a v = (e, a, v, Assert)

-- | Retract a fact.
retract :: EntityId -> Attr -> Value -> (EntityId, Attr, Value, Op)
retract e a v = (e, a, v, Retract)

-- | A committed transaction: its id, its time if it was stamped and the
-- datoms it wrote.
data TxReport = TxReport
  { trTxId   :: TxId
  , trTime   :: Maybe UTCTime
  , trDatoms :: [Datom]
  } deriving (Eq, Show)

-- | Apply raw writes. Datoms go in as given with no uniqueness checks... those
-- live in 'Datomlite.Schema.commit'.
transact :: TxData -> Db -> (TxReport, Db)
transact ops db = (TxReport (pendingTx db) Nothing (stamp ops db), closeTx (stage ops db))

-- The id the next commit will get
pendingTx :: Db -> TxId
pendingTx db = TxId (dbNextTx db)

stamp :: TxData -> Db -> [Datom]
stamp ops db = [ Datom e a v (pendingTx db) o | (e, a, v, o) <- ops ]

-- | Write datoms under the pending tx without closing it. A transaction can be
-- built in steps and still commit as one.
stage :: TxData -> Db -> Db
stage ops db = foldl' index db { dbLog = reverse ds ++ dbLog db } ds
  where ds = stamp ops db

-- | Log a wall-clock time for the pending transaction. Never earlier than the
-- last one so times only go forward even if the clock doesn't.
stampTime :: UTCTime -> Db -> Db
stampTime t db = stage [(EntityId 0, txInstant, VInstant t', Assert)] db
  where t' = maybe t (max t . snd) (Map.lookupMax (dbTimes db))

-- | Close the pending transaction: everything staged becomes that tx.
closeTx :: Db -> Db
closeTx db = db { dbBasis = pendingTx db, dbNextTx = dbNextTx db + 1 }

-- | The database as of a transaction (that one included).
asOf :: TxId -> Db -> Db
asOf tx db = (reindex (filter ((<= tx) . dTx) (dbLog db)) db) { dbBasis = min tx (dbBasis db) }

-- | The database as of a wall-clock time: up to the last transaction stamped
-- at or before @t@. An unstamped one (a pure 'Datomlite.Schema.commit') goes
-- with the stamp before it.
asOfTime :: UTCTime -> Db -> Db
asOfTime t db = case [ tx | (tx, u) <- Map.toAscList (dbTimes db), u > t ] of
  (TxId k : _) -> asOf (TxId (k - 1)) db
  []           -> db

-- | Only the facts written after a transaction.
--
-- An entity written before @tx@ and changed after only has part of its facts
-- here... so 'Datomlite.load' and queries just see the entities that are whole
-- in the window.
since :: TxId -> Db -> Db
since tx db = (reindex (filter ((> tx) . dTx) (dbLog db)) db) { dbPartial = True }

-- | Every datom ever asserted or retracted, oldest first.
history :: Db -> [Datom]
history = reverse . dbLog

-- | Rebuild a database from its log. The inverse of 'history' and how you
-- restore a saved one:
--
-- @
-- writeFile \"db.log\" (unlines (map show (history db)))
-- db <- fromHistory . map read . lines \<$\> readFile \"db.log\"
-- @
--
-- Datoms replay in tx order and keep their order within a tx. New entity ids
-- start past every id in the log (refs included). A trailing empty transaction
-- is lost... so 'basisT' is the last one that wrote something.
fromHistory :: [Datom] -> Db
fromHistory ds =
  db { dbBasis = TxId lastTx, dbNextTx = lastTx + 1, dbNextE = lastE + 1 }
  where
    db     = reindex (reverse (sortOn dTx ds)) emptyDb
    lastTx = maximum (0 : [ t | Datom { dTx = TxId t } <- ds ])
    lastE  = maximum (0 : concat [ e : refTo (dValue d)
                                 | d@Datom { dEntity = EntityId e } <- ds ])
    refTo (VRef (EntityId r)) = [r]
    refTo _                   = []

-- | The live datoms grouped by entity.
datoms :: Db -> [Datom]
datoms db = concatMap concat (Map.elems (Map.map Map.elems (dbEavt db)))

-- | An entity's live values by attribute. A many-valued attribute lists its
-- values in the order they were asserted.
entityAttrs :: EntityId -> Db -> Map Attr [Value]
entityAttrs e db = maybe Map.empty (Map.map (map dValue)) (Map.lookup e (dbEavt db))

-- | An entity's value for an attribute.
lookupValue :: EntityId -> Attr -> Db -> Maybe Value
lookupValue e a db = case lookupValues e a db of
  (v:_) -> Just v
  []    -> Nothing

-- | All of an entity's values for an attribute.
lookupValues :: EntityId -> Attr -> Db -> [Value]
lookupValues e a db = Map.findWithDefault [] a (entityAttrs e db)

-- | The entities holding a value for an attribute. An AVET lookup.
entitiesWith :: Attr -> Value -> Db -> [EntityId]
entitiesWith a v db = Set.toList (entitiesWithSet a v db)

entitiesWithSet :: Attr -> Value -> Db -> Set EntityId
entitiesWithSet a v db =
  maybe Set.empty (Map.findWithDefault Set.empty v) (Map.lookup a (dbAvet db))

-- | The entities holding any attribute in a namespace (any @Employee\/…@ for
-- @"Employee"@). In creation order.
entitiesInNamespace :: Text -> Db -> [EntityId]
entitiesInNamespace ns db =
  Set.toList . Set.unions . concatMap Map.elems . Map.elems
    . Map.takeWhileAntitone (prefix `T.isPrefixOf`)
    . Map.dropWhileAntitone (< prefix)
    $ dbAvet db
  where prefix = ns <> "/"

-- | The entity holding a value for an attribute. Meant for unique attributes
-- where there's at most one.
uniqueAt :: Attr -> Value -> Db -> Maybe EntityId
uniqueAt a v db = case entitiesWith a v db of
  (e:_) -> Just e
  []    -> Nothing

-- | A connection: the current 'Db' in a 'TVar' for readers and a lock writers
-- take turns on.
data Conn = Conn (TVar Db) (MVar ())

-- | Open a connection on a starting 'Db'.
connect :: Db -> IO Conn
connect db = Conn <$> newTVarIO db <*> newMVar ()

-- | The connection's 'Db' right now. A value, so later commits don't touch it.
snapshot :: Conn -> IO Db
snapshot (Conn v _) = readTVarIO v

-- | Run a pure step on the connection's 'Db' and swap in the result.
-- 'Datomlite.Schema.transact' is built on this.
--
-- Writers take turns and the step is fully evaluated before the swap. A step
-- never re-runs and readers only ever see a finished 'Db'. A step that throws
-- leaves the connection as it was.
atomicUpdate :: Conn -> (Db -> (Db, r)) -> IO r
atomicUpdate (Conn v lock) f = withMVar lock $ \() -> do
  (db', r) <- f <$> readTVarIO v
  _  <- evaluate db'
  r' <- evaluate r
  atomically (writeTVar v db')
  pure r'

-- | Block until the connection is past a transaction and return its 'Db'.
-- It's just 'STM' so it composes with 'Control.Concurrent.STM.orElse' or a
-- timeout.
--
-- @
-- db' <- atomically (awaitAfter conn (basisT db))
-- @
awaitAfter :: Conn -> TxId -> STM Db
awaitAfter (Conn v _) t = do
  db <- readTVar v
  check (dbBasis db > t)
  pure db

-- | One 'TxReport' per transaction after the given one (oldest first). An empty
-- transaction gets an empty report.
txsAfter :: TxId -> Db -> [TxReport]
txsAfter t@(TxId from) db =
  [ TxReport (TxId i) (txTime (TxId i) db) (Map.findWithDefault [] (TxId i) byTx)
  | i <- [from + 1 .. to] ]
  where
    TxId to = dbBasis db
    byTx = Map.fromList [ (dTx (NE.head g), NE.toList g) | g <- NE.groupWith dTx newer ]
    newer = reverse (takeWhile ((> t) . dTx) (dbLog db))
