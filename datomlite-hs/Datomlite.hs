{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RoleAnnotations        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

-- |
-- A lightweight immutable database. The 'Db' is a plain value... 'commit'
-- hands back a new one and every past state stays queryable.
--
-- Records are the schema. A field's type says how it is stored and
-- 'Identity' names the fields that identify an entity:
--
-- @
-- \{\-\# LANGUAGE DeriveGeneric, DeriveAnyClass, DataKinds, TypeFamilies \#\-\}
-- \{\-\# LANGUAGE OverloadedRecordDot, OverloadedStrings \#\-\}
-- import Datomlite
--
-- data Department = Department { code :: Text, deptName :: Text }
--   deriving Generic
-- instance Entity Department where
--   type Identity Department = '[\"code\"]
--
-- data Employee = Employee
--   { email  :: Text             -- one value
--   , name   :: Text
--   , dept   :: Ref Department   -- another entity
--   , skills :: Set Text         -- many values
--   , nick   :: Maybe Text       -- optional
--   } deriving Generic
-- instance Entity Employee where
--   type Identity Employee = '[\"email\"]   -- unique, and what 'upsert' goes by
-- @
--
-- Writes are a list of 'Write's that commit all at once. A 'ref' names an
-- entity by its identity, so records in one list can point at each other:
--
-- @
-- commit_ db
--   [ Upsert (Department \"ENG\" \"Engineering\")
--   , Upsert (Employee \"m\@x.io\" \"Max\" (ref \@Department \"ENG\") mempty Nothing)
--   ]
-- @
--
-- A 'Tx' do-block ('insert', 'upsert', 'update', 'retract') is for steps that
-- need each other's results. 'commit' takes either.
--
-- Every read ('load', 'every', 'find' and queries) hands back a 'Stored'
-- record: @p.name@ reads a field and 'refOf' gives its 'Ref'.
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
-- A wrong field or type is a compile error. 'match' scans... 'follow' and
-- 'matchBy' go straight to the entity.
--
-- The log is open: 'history' and 'datoms' hand back every 'Datom', and
-- 'fromHistory' builds a 'Db' from them.
module Datomlite
  ( -- * The database value
    Db
  , emptyDb
  , TxId (..)
  , basisT
  , asOf
  , asOfTime
  , txTime
  , since

    -- * The log: every fact, ever
  , history
  , fromHistory
  , datoms
  , Datom (..)
  , Op (..)
  , refId

    -- * Schema: records are the schema
  , Entity (Identity, ename)
  , Ref
  , ref
  , refBy
  , ToValue (..)
  , FromValue (..)

    -- * Writing: build a 'Tx', then commit it
  , Tx
  , insert
  , insert_
  , upsert
  , upsert_
  , update
  , setField
  , retract
  , txDb
  , abort
  , Write (..)
  , Writes (Result)
  , commit
  , commit_
  , commitAt
  , TxError (..)
  , Attr
  , Value (..)
  , EntityId (..)

    -- * Reading records
  , Stored
  , refOf
  , recordOf
  , load
  , every
  , find
  , findBy
  , LoadError (..)

    -- * Querying: do-blocks over records
  , Query
  , query
  , match
  , matchBy
  , follow
  , oneOf
  , closure
  , exists
  , none

    -- * Window functions over results
  , Window
  , window
  , rowNumber
  , rank
  , denseRank
  , lag
  , lead
  , whole
  , running

    -- * The IO edge
  , Conn
  , connect
  , snapshot
  , transact

    -- * Reacting to commits
  , listen
  , watch
  , awaitAfter
  , TxReport (..)
  ) where

import           Control.Applicative     (Alternative (..))
import           Control.Concurrent      (forkIO)
import           Control.Concurrent.MVar (MVar, newMVar, withMVar)
import           Control.Concurrent.STM  (STM, TVar, atomically, check, newTVarIO, orElse,
                                          readTVar, readTVarIO, writeTVar)
import           Control.Exception       (Exception (..), SomeAsyncException, SomeException,
                                          catch, evaluate, fromException, throw, throwIO)
import           Control.Monad           (MonadPlus, ap, forM_, unless, void, when)
import           Data.ByteString         (ByteString)
import qualified Data.ByteString         as BS
import           Data.Foldable           (toList, traverse_)
import           Data.IORef              (newIORef, readIORef, writeIORef)
import           Data.Kind               (Constraint, Type)
import           Data.List               (group, inits, nub, sortBy, sortOn)
import qualified Data.List               as List
import           Data.Map.Strict         (Map)
import qualified Data.Map.Strict         as Map
import           Data.Maybe              (isJust)
import           Data.Ord                (comparing)
import           Data.Proxy              (Proxy (..))
import           Data.Sequence           (Seq (..), (><))
import qualified Data.Sequence           as Seq
import           Data.Set                (Set)
import qualified Data.Set                as Set
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T
import           Data.Time               (Day, UTCTime (..), getCurrentTime)
import           Data.Type.Bool          (If, type (&&), type (||))
import           GHC.Generics
import           GHC.Records             (HasField (..))
import           GHC.TypeLits            (ErrorMessage (..), KnownSymbol, Symbol, TypeError,
                                          symbolVal)
import           System.IO               (stderr)

--------------------------------------------------------------------------------
-- The log and the Db

-- | An entity's id. 'insert' hands them out and they're never reused. The
-- constructor is there for writing your own encoding of the log.
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
    -- 'ref' makes one and commit swaps in the 'VRef' it names, so it's never
    -- stored.
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
        VInt _      -> 0
        VText _     -> 1
        VBool _     -> 2
        VRef _      -> 3
        VDouble _   -> 4
        VInteger _  -> 5
        VBytes _    -> 6
        VInstant _  -> 7
        VDay _      -> 8
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

-- | When a transaction was committed, if it was stamped (see 'commitAt').
txTime :: TxId -> Db -> Maybe UTCTime
txTime tx db = Map.lookup tx (dbTimes db)

-- The attribute a tx's wall-clock time is logged under
txInstant :: Attr
txInstant = "db/txInstant"

-- Fold one datom into the live indexes. Asserting a live fact again or
-- retracting a dead one is a no-op. A tx time goes to 'dbTimes' and not the
-- indexes... it's about the tx and not an entity
index :: Db -> Datom -> Db
index db d@(Datom e a v tx op)
  | a == txInstant, VInstant t <- v = db { dbTimes = Map.insert tx t (dbTimes db) }
  | otherwise = case op of
      Assert  | not live -> db { dbEavt = addFact (dbEavt db),  dbAvet = addHolder (dbAvet db) }
      Retract | live     -> db { dbEavt = dropFact (dbEavt db), dbAvet = dropHolder (dbAvet db) }
      _                  -> db
  where
    live       = e `Set.member` holders a v db
    addFact    = Map.insertWith (Map.unionWith (flip (++))) e (Map.singleton a [d])
    addHolder  = Map.insertWith (Map.unionWith Set.union) a (Map.singleton v (Set.singleton e))
    dropFact   = Map.update (nonEmpty . Map.update (nonEmpty . filter ((/= v) . dValue)) a) e
    dropHolder = Map.update (nonEmpty . Map.update (nonEmpty . Set.delete e) v) a

-- Map.update and friends delete a key whose value comes back Nothing
nonEmpty :: Foldable t => t x -> Maybe (t x)
nonEmpty xs
  | null xs   = Nothing
  | otherwise = Just xs

-- Rebuild a database from a log (newest first). The id counters are kept
reindex :: [Datom] -> Db -> Db
reindex lg db =
  List.foldl' index db { dbLog = lg, dbEavt = Map.empty, dbAvet = Map.empty, dbTimes = Map.empty }
    (reverse lg)

-- Datoms still missing their tx id. Commit stamps it on
type TxData = [(EntityId, Attr, Value, Op)]

-- | A committed transaction: its id, its time if it was stamped and the
-- datoms it wrote.
data TxReport = TxReport
  { trTxId   :: TxId
  , trTime   :: Maybe UTCTime
  , trDatoms :: [Datom]
  } deriving (Eq, Show)

-- The id the next commit will get
pendingTx :: Db -> TxId
pendingTx db = TxId (dbNextTx db)

-- Write datoms under the pending tx without closing it. A transaction can be
-- built in steps and still commit as one
stage :: TxData -> Db -> Db
stage ops db = List.foldl' index db { dbLog = reverse ds ++ dbLog db } ds
  where ds = [ Datom e a v (pendingTx db) o | (e, a, v, o) <- ops ]

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
-- at or before @t@. An unstamped one (a pure 'commit') goes with the stamp
-- before it.
asOfTime :: UTCTime -> Db -> Db
asOfTime t db = case [ tx | (tx, u) <- Map.toAscList (dbTimes db), u > t ] of
  (TxId k : _) -> asOf (TxId (k - 1)) db
  []           -> db

-- | Only the facts written after a transaction.
--
-- An entity written before @tx@ and changed after only has part of its facts
-- here... so 'load' and queries just see the entities that are whole in the
-- window.
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
datoms db = concatMap (concat . Map.elems) (Map.elems (dbEavt db))

-- | An entity's live values by attribute. A many-valued attribute lists its
-- values in the order they were asserted.
entityAttrs :: EntityId -> Db -> Map Attr [Value]
entityAttrs e db = maybe Map.empty (Map.map (map dValue)) (Map.lookup e (dbEavt db))

-- | All of an entity's values for an attribute.
lookupValues :: EntityId -> Attr -> Db -> [Value]
lookupValues e a db = Map.findWithDefault [] a (entityAttrs e db)

-- | The entities holding a value for an attribute. An AVET lookup.
entitiesWith :: Attr -> Value -> Db -> [EntityId]
entitiesWith a v db = Set.toList (holders a v db)

holders :: Attr -> Value -> Db -> Set EntityId
holders a v db = maybe Set.empty (Map.findWithDefault Set.empty v) (Map.lookup a (dbAvet db))

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
uniqueAt a v db = Set.lookupMin (holders a v db)

-- | A connection: the current 'Db' in a 'TVar' for readers and a lock writers
-- take turns on.
data Conn = Conn (TVar Db) (MVar ())

-- | Open a connection on a starting 'Db'.
connect :: Db -> IO Conn
connect db = Conn <$> newTVarIO db <*> newMVar ()

-- | The connection's 'Db' right now. A value, so later commits don't touch it.
snapshot :: Conn -> IO Db
snapshot (Conn v _) = readTVarIO v

-- | Run a pure step on the connection's 'Db' and swap in the result. 'transact'
-- is built on this.
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
txsAfter t@(TxId after) db = [ report (TxId i) | i <- [after + 1 .. latest] ]
  where
    TxId latest = dbBasis db
    report tx   = TxReport tx (txTime tx db) (Map.findWithDefault [] tx byTx)
    -- walking the newest-first log and prepending puts each tx's datoms back in order
    byTx = Map.fromListWith (++) [ (dTx d, [d]) | d <- takeWhile ((> t) . dTx) (dbLog db) ]

--------------------------------------------------------------------------------
-- Schema

-- Records as the schema. Field types give cardinality and 'Identity' names
-- the unique fields... a Generic-derived 'Entity' shreds a record into datoms
-- and back.

-- | A typed reference to another entity: @dept :: Ref Department@.
--
-- One from the database ('insert', 'upsert', a read) holds the entity's id so
-- it always points at the type it claims. 'ref' makes a lookup ref instead:
-- the entity holding an 'Identity' value, found when it's written or read.
-- Its role is nominal so 'coerce' can't cheat either.
--
-- Refs compare by what they hold, so a lookup ref never equals the id it
-- resolves to. Compare refs from the database: @guard (e.dept == refOf d)@.
data Ref a
  = Ref EntityId
  | LookupRef Attr Value
  deriving (Eq, Ord, Show)
type role Ref nominal

-- | The 'EntityId' behind a 'Ref' from the database. A lookup ref has none
-- until it's resolved, so this throws on one: read it first with 'load'.
refId :: Ref a -> EntityId
refId (Ref e) = e
refId (LookupRef a v) =
  error ("datomlite: refId of the lookup ref " <> T.unpack a <> " " <> show v
         <> ", which has no id until it's resolved. load it and use refOf")

-- The entity a ref names in a database, if any
resolve :: Ref a -> Db -> Maybe EntityId
resolve (Ref e)         _  = Just e
resolve (LookupRef a v) db = uniqueAt a v db

-- | Scalars that can be stored as a 'Value'. Enums derive one (stored by
-- constructor name) and newtypes can borrow the wrapped type's:
--
-- @
-- data Status = Active | Banned
--   deriving stock (Generic, Eq, Ord)
--   deriving anyclass (ToValue, FromValue)
--
-- newtype Email = Email Text
--   deriving newtype (Eq, Ord, ToValue, FromValue)
-- @
class ToValue a where
  toValue :: a -> Value
  default toValue :: (Generic a, GEnum (Rep a)) => a -> Value
  toValue = VText . genumName . from

-- | The inverse of 'toValue'.
class FromValue a where
  fromValue :: Value -> Maybe a
  default fromValue :: (Generic a, GEnum (Rep a)) => Value -> Maybe a
  fromValue (VText t) = to <$> genumFrom t
  fromValue _         = Nothing

instance ToValue Text where toValue = VText
instance FromValue Text where
  fromValue (VText t) = Just t
  fromValue _         = Nothing

instance ToValue Int where toValue = VInt
instance FromValue Int where
  fromValue (VInt n) = Just n
  fromValue _        = Nothing

instance ToValue Bool where toValue = VBool
instance FromValue Bool where
  fromValue (VBool b) = Just b
  fromValue _         = Nothing

instance ToValue (Ref t) where
  toValue (Ref e)         = VRef e
  toValue (LookupRef a v) = VLookup a v
instance FromValue (Ref t) where
  fromValue (VRef e) = Just (Ref e)
  fromValue _        = Nothing

instance ToValue Double where toValue = VDouble
instance FromValue Double where
  fromValue (VDouble d) = Just d
  fromValue _           = Nothing

instance ToValue Integer where toValue = VInteger
instance FromValue Integer where
  fromValue (VInteger n) = Just n
  fromValue _            = Nothing

instance ToValue ByteString where toValue = VBytes
instance FromValue ByteString where
  fromValue (VBytes b) = Just b
  fromValue _          = Nothing

-- 'UTCTime''s fields are lazy. Force them since everything in a 'Db' is
-- evaluated
instance ToValue UTCTime where
  toValue t@(UTCTime d s) = d `seq` s `seq` VInstant t
instance FromValue UTCTime where
  fromValue (VInstant t) = Just t
  fromValue _            = Nothing

instance ToValue Day where toValue = VDay
instance FromValue Day where
  fromValue (VDay d) = Just d
  fromValue _        = Nothing

-- Generic enums: a constructor with no fields is stored as its name
class GEnum f where
  genumName :: f p -> Text
  genumFrom :: Text -> Maybe (f p)

instance GEnum f => GEnum (M1 D d f) where
  genumName (M1 x) = genumName x
  genumFrom t      = M1 <$> genumFrom t

instance (GEnum f, GEnum g) => GEnum (f :+: g) where
  genumName (L1 x) = genumName x
  genumName (R1 y) = genumName y
  genumFrom t      = L1 <$> genumFrom t <|> R1 <$> genumFrom t

instance Constructor c => GEnum (M1 C c U1) where
  genumName _ = T.pack (conName (undefined :: M1 C c U1 p))
  genumFrom t
    | t == genumName (undefined :: M1 C c U1 p) = Just (M1 U1)
    | otherwise                                  = Nothing

instance TypeError (NotEnum name) => GEnum (M1 C ('MetaCons name fx s) (M1 S m f)) where
  genumName = error "unreachable"; genumFrom = error "unreachable"
instance TypeError (NotEnum name) => GEnum (M1 C ('MetaCons name fx s) (f :*: g)) where
  genumName = error "unreachable"; genumFrom = error "unreachable"

type NotEnum name =
  'Text "datomlite: only enums derive ToValue/FromValue, and constructor "
    ':<>: 'Text name ':<>: 'Text " has fields."
    ':$$: 'Text "Store a record as its own Entity and point at it with a Ref."

-- Without this a 'String' field reads as many 'Char's... and fails with a
-- confusing missing instance
type NoChar =
  'Text "datomlite: String/Char fields aren't supported; use Data.Text.Text"

instance TypeError NoChar => ToValue Char where
  toValue = error "unreachable"
instance TypeError NoChar => FromValue Char where
  fromValue = error "unreachable"

-- | How a single field is split into datoms.
data Shape
  = ShapeOne       -- ^ exactly one value
  | ShapeOptional  -- ^ zero or one value
  | ShapeMany      -- ^ zero or more values
  deriving (Eq, Show)

-- | How a field type is stored: its 'Shape' and how to turn it into 'Value's
-- and back. Instances cover 'Maybe', 'Set' and plain scalars (including
-- 'Ref').
class Field a where
  shape   :: Proxy a -> Shape
  shred   :: a -> [Value]
  unshred :: [Value] -> Maybe a

-- A plain scalar. Overlappable so the instances below win for 'Maybe', 'Set'
-- and lists
instance {-# OVERLAPPABLE #-} (ToValue a, FromValue a) => Field a where
  shape _ = ShapeOne
  shred a = [toValue a]
  unshred (v:_) = fromValue v
  unshred []    = Nothing

instance (ToValue a, FromValue a) => Field (Maybe a) where
  shape _ = ShapeOptional
  shred Nothing  = []
  shred (Just a) = [toValue a]
  unshred []    = Just Nothing
  unshred (v:_) = Just <$> fromValue v

instance (Ord a, ToValue a, FromValue a) => Field (Set a) where
  shape _ = ShapeMany
  shred   = map toValue . Set.toList
  unshred = fmap Set.fromList . traverse fromValue

-- A many-valued field keeps no order or duplicates... so a list would promise
-- more than the store gives back. Point at 'Set' instead (or 'Text' for a
-- 'String')
instance TypeError (ListField a) => Field [a] where
  shape   = error "unreachable"
  shred   = error "unreachable"
  unshred = error "unreachable"

type family ListField a :: ErrorMessage where
  ListField Char = NoChar
  ListField a    =
    'Text "datomlite: a many-valued field is a set, so declare it as (Set "
      ':<>: 'ShowType a ':<>: 'Text "), not [" ':<>: 'ShowType a ':<>: 'Text "]"

-- | A record that can be stored as datoms and read back. Derive it:
--
-- @
-- \{\-\# LANGUAGE DeriveGeneric, DeriveAnyClass \#\-\}
--
-- data Employee = Employee { email :: Text, name :: Text, ... }
--   deriving (Generic, Entity)
-- @
--
-- or write an @instance Entity Employee@ to name its 'Identity' fields:
--
-- @
-- instance Entity Employee where
--   type Identity Employee = '[\"email\"]
-- @
class Entity a where
  -- | The fields that identify an entity. Each one is unique on its own: no
  -- two entities hold the same value and 'upsert' finds the entity by it. A
  -- name the record doesn't have is a compile error. Defaults to none.
  type Identity a :: [Symbol]
  type Identity a = '[]

  -- | The @Type@ in its @Type\/field@ attributes. Defaults to the type name.
  ename :: Proxy a -> Text
  default ename :: (Generic a, GName (Rep a)) => Proxy a -> Text
  ename _ = gname (Proxy @(Rep a))

  -- | Shred a record into (attribute, values, shape) triples.
  decompose :: a -> [(Attr, [Value], Shape)]
  default decompose
    :: (Generic a, GEntity (Rep a)) => a -> [(Attr, [Value], Shape)]
  decompose = gdecompose (ename (Proxy @a)) . from

  -- | Rebuild a record from an entity's values or say which field didn't
  -- decode.
  reconstruct :: EntityId -> Map.Map Attr [Value] -> Either LoadError a
  default reconstruct
    :: (Generic a, GEntity (Rep a))
    => EntityId -> Map.Map Attr [Value] -> Either LoadError a
  reconstruct e m = to <$> greconstruct (ename (Proxy @a)) e m

  -- | The attributes of the 'Identity' fields:
  -- @["Employee/email"]@.
  identityAttrs :: Proxy a -> [Attr]
  default identityAttrs
    :: (IdentityFields (Identity a) a, KnownSymbols (Identity a))
    => Proxy a -> [Attr]
  identityAttrs p = map (attrName (ename p)) (symbolVals @(Identity a))

-- Every name in an 'Identity' list is a field of the record
type family IdentityFields (names :: [Symbol]) a :: Constraint where
  IdentityFields '[]       a = ()
  IdentityFields (n ': ns) a =
    (IdentityField (HasSel n (Rep a)) n a, IdentityFields ns a)

type family IdentityField (found :: Bool) (name :: Symbol) a :: Constraint where
  IdentityField 'True  name a = ()
  IdentityField 'False name a = TypeError
    ('Text "datomlite: Identity " ':<>: 'ShowType a ':<>: 'Text " names "
       ':<>: 'ShowType name ':<>: 'Text " but " ':<>: 'ShowType a
       ':<>: 'Text " has no such field")

-- A type-level list of names, read back as strings
class KnownSymbols (names :: [Symbol]) where
  symbolVals :: [String]

instance KnownSymbols '[] where
  symbolVals = []

instance (KnownSymbol n, KnownSymbols ns) => KnownSymbols (n ': ns) where
  symbolVals = symbolVal (Proxy @n) : symbolVals @ns

-- Datatype name
class GName f where
  gname :: Proxy f -> Text

instance (Datatype d) => GName (M1 D d f) where
  gname _ = T.pack (datatypeName (undefined :: M1 D d f p))

-- Shredding and rebuilding over the generic representation
class GEntity f where
  gdecompose    :: Text -> f p -> [(Attr, [Value], Shape)]
  greconstruct  :: Text -> EntityId -> Map.Map Attr [Value] -> Either LoadError (f p)

instance (OneRecord ty f, GEntity f) => GEntity (M1 D ('MetaData ty m p nt) f) where
  gdecompose n (M1 x)   = gdecompose n x
  greconstruct n e m    = M1 <$> greconstruct n e m

-- Shapes 'OneRecord' rules out. Never used since the check fails to compile
-- first
instance GEntity (f :+: g) where
  gdecompose = error "unreachable"; greconstruct = error "unreachable"
instance GEntity U1 where
  gdecompose = error "unreachable"; greconstruct = error "unreachable"
instance GEntity V1 where
  gdecompose = error "unreachable"; greconstruct = error "unreachable"

-- An entity is one constructor with named fields since each name is an
-- attribute. Anything else gets a type error that says so
type family OneRecord (ty :: Symbol) (f :: Type -> Type) :: Constraint where
  OneRecord ty (M1 C c U1) = TypeError
    ('Text "datomlite: " ':<>: 'Text ty ':<>: 'Text " has no fields to store.")
  OneRecord ty (M1 C c f) = If (AllNamed f) (() :: Constraint) (TypeError
    ('Text "datomlite: " ':<>: 'Text ty ':<>: 'Text "'s fields need names, since each one is an attribute."
       ':$$: 'Text "Declare it with record syntax: data " ':<>: 'Text ty ':<>: 'Text " = "
       ':<>: 'Text ty ':<>: 'Text " { field :: ..., ... }"))
  OneRecord ty V1 = TypeError
    ('Text "datomlite: " ':<>: 'Text ty ':<>: 'Text " has no constructors to store.")
  OneRecord ty f = TypeError
    ('Text "datomlite: " ':<>: 'Text ty ':<>: 'Text " has several constructors; an entity is one record."
       ':$$: 'Text "Split it into one record per case, or store the case in an enum field.")

type family AllNamed (f :: Type -> Type) :: Bool where
  AllNamed (f :*: g) = AllNamed f && AllNamed g
  AllNamed (M1 S ('MetaSel ('Just n) su ss ds) f) = 'True
  AllNamed f = 'False

instance GEntity f => GEntity (M1 C c f) where
  gdecompose n (M1 x)   = gdecompose n x
  greconstruct n e m    = M1 <$> greconstruct n e m

instance (GEntity f, GEntity g) => GEntity (f :*: g) where
  gdecompose n (x :*: y) = gdecompose n x ++ gdecompose n y
  greconstruct n e m     = (:*:) <$> greconstruct n e m <*> greconstruct n e m

instance (Selector s, Field a) => GEntity (M1 S s (K1 i a)) where
  gdecompose n s@(M1 (K1 x)) = [(attrName n (selName s), shred x, shape (Proxy @a))]

  greconstruct n e m = case unshred vs of
      Just x              -> Right (M1 (K1 x))
      Nothing | null vs   -> Left (MissingField attr e)
              | otherwise -> Left (BadValue attr vs e)
    where
      attr = attrName n (selName (undefined :: M1 S s (K1 i a) p))
      vs   = Map.findWithDefault [] attr m

-- | The attribute for a field: @Type\/field@.
attrName :: Text -> String -> Attr
attrName ty fld = ty <> "/" <> T.pack fld

--------------------------------------------------------------------------------
-- Writing

-- A record's fields with every lookup ref swapped for the entity it names
fieldsIn :: Entity a => Db -> a -> Either TxError [(Attr, [Value], Shape)]
fieldsIn db = traverse resolveField . decompose
  where
    resolveField (attr, vs, sh) = (\vs' -> (attr, nub vs', sh)) <$> traverse resolveValue vs
    resolveValue (VLookup a v) = VRef <$> holderOf a v db
    resolveValue v             = Right v

-- The entity a ref names, or why there isn't one
target :: Ref a -> Db -> Either TxError EntityId
target (Ref e)         _  = Right e
target (LookupRef a v) db = holderOf a v db

-- The entity holding an identity value
holderOf :: Attr -> Value -> Db -> Either TxError EntityId
holderOf a v db = maybe (Left (UnknownIdentity a v)) Right (uniqueAt a v db)

-- Make entity @e@ match a record's fields: stage only the difference and
-- claim the identity values it newly holds
write :: [Attr] -> EntityId -> [(Attr, [Value], Shape)] -> TxState -> TxState
write ids e fields s = emit (retracts ++ asserts) claims s
  where
    changes  = [ (attr, lookupValues e attr (txView s), new) | (attr, new, _) <- fields ]
    retracts = [ (e, attr, v, Retract) | (attr, old, new) <- changes, v <- old, v `notElem` new ]
    asserts  = [ (e, attr, v, Assert)  | (attr, old, new) <- changes, v <- new, v `notElem` old ]
    claims   = [ (attr, v) | (_, attr, v, _) <- asserts, attr `elem` ids ]

-- | Why a commit failed. Nothing is written.
data TxError
  = -- | Several entities would hold the same 'Identity' value.
    UniqueViolation
      { uvAttr     :: Attr       -- ^ the identity attribute, e.g. @"Employee/email"@
      , uvValue    :: Value      -- ^ the value they would share
      , uvEntities :: [EntityId] -- ^ the entities holding it, oldest first
      }
    -- | A lookup ref (see 'ref') that no entity holds.
  | UnknownIdentity Attr Value
    -- | 'update' on an entity with no facts.
  | UnknownEntity EntityId
    -- | The transaction gave up through 'abort' or a failed pattern match.
  | Aborted Text
  deriving (Eq, Show)

-- | A transaction. The 'Ref' from 'insert' works right away in later steps and
-- the whole thing commits at once:
--
-- @
-- Right (empRef, db') = 'commit' db $ do
--   eng <- 'insert' (Department { code = \"ENG\", deptName = \"Engineering\" })
--   'insert' (Employee { email = \"m\@x.io\", dept = eng, ... })
-- @
--
-- When nothing needs passing along, a list of 'Write's says the same thing
-- more plainly.
--
-- Each step sees the ones before it and 'txDb' reads that in-progress
-- database. Small 'Tx's compose into bigger ones (it's a 'Monad' and a
-- 'Monoid' when the result is).
--
-- Under the hood it's the 'Db' being written (every step staged under one
-- pending tx) plus the identity claims to check on commit. Ids are handed out as
-- you go... so a tempid is just a real 'EntityId'.
newtype Tx a = Tx { runTx :: TxState -> Either TxError (a, TxState) }

data TxState = TxState
  { txView   :: Db -- the db with this tx's writes so far staged
  , txClaims :: [(Attr, Value)] -- identity values that must end up unique
  }

instance Functor Tx where
  fmap f (Tx g) = Tx $ \s -> fmap (\(a, s') -> (f a, s')) (g s)

instance Applicative Tx where
  pure x = Tx $ \s -> Right (x, s)
  (<*>)  = ap

instance Monad Tx where
  Tx g >>= k = Tx $ \s -> g s >>= \(a, s') -> runTx (k a) s'

-- | A failed pattern match aborts the whole transaction.
instance MonadFail Tx where
  fail = abort . T.pack

instance Semigroup a => Semigroup (Tx a) where
  x <> y = (<>) <$> x <*> y

instance Monoid a => Monoid (Tx a) where
  mempty = pure mempty

-- Stage datoms and record unique claims
emit :: TxData -> [(Attr, Value)] -> TxState -> TxState
emit ops claims s = TxState (stage ops (txView s)) (claims ++ txClaims s)

-- | Insert a record as a new entity. The 'Ref' it returns works right away.
-- An 'Identity' value another entity already holds fails the commit with
-- 'UniqueViolation' (use 'upsert' to update that one instead).
insert :: forall a. Entity a => a -> Tx (Ref a)
insert a = Tx $ \s -> do
  fields <- fieldsIn (txView s) a
  let (e, view) = freshEntity (txView s)
  pure (Ref e, write (identityAttrs (Proxy @a)) e fields s { txView = view })

-- | 'insert' without the 'Ref'. For a do-block of writes.
insert_ :: Entity a => a -> Tx ()
insert_ = void . insert

-- | Insert, or update the entity already holding one of the record's
-- 'Identity' values. Either way you get the 'Ref' back:
--
-- @
-- commit db $ upsert (Department \"ENG\" \"Platform Engineering\")
-- @
--
-- Only the difference is written. A record with no 'Identity' is always
-- inserted. Identity values held by two different entities fail with
-- 'UniqueViolation'.
upsert :: forall a. Entity a => a -> Tx (Ref a)
upsert a = Tx $ \s -> do
  fields <- fieldsIn (txView s) a
  let ids     = identityAttrs (Proxy @a)
      owners = [ e | (attr, vs, _) <- fields, attr `elem` ids
                   , v <- vs, e <- entitiesWith attr v (txView s) ]
  case owners of
    []    -> runTx (insert a) s
    (e:_) -> pure (Ref e, write ids e fields s)

-- | 'upsert' without the 'Ref'. For a do-block of writes.
upsert_ :: Entity a => a -> Tx ()
upsert_ = void . upsert

-- | Change an entity with a function on its record:
--
-- @
-- commit_ db $ update maxRef (\\e -> e { name = \"Maximilian\" })
-- @
--
-- Only the difference is written so a one-valued field gets replaced (not
-- doubled up). Changed 'Identity' fields are checked for uniqueness. Fails with
-- 'UnknownEntity' if the entity has no facts (or 'UnknownIdentity' for a
-- 'ref' nothing holds).
update :: forall a. Entity a => Ref a -> (a -> a) -> Tx ()
update r f = Tx $ \s -> do
  e      <- target r (txView s)
  old    <- maybe (Left (UnknownEntity e)) Right (loadRecord e (txView s))
  fields <- fieldsIn (txView s) (f old)
  pure ((), write (identityAttrs (Proxy @a)) e fields s)

-- | Set one field by name. Handy with 'update':
--
-- @
-- commit_ db $ update maxRef (setField \@\"name\" \"Maximilian\")
-- @
--
-- Record-update syntax works too but gets ambiguous when two records share a
-- field name. Here the record type comes from the 'Ref'. Field and type are
-- checked like @e.name@.
setField
  :: forall name a t
   . (HasNamedField name a, HasField name a t, Generic a, GSetField name t (Rep a))
  => t -> a -> a
setField v = to . gsetField @name v . from

-- Replace the field called @name@ in a generic representation
class GSetField (name :: Symbol) t (f :: Type -> Type) where
  gsetField :: t -> f p -> f p

instance GSetField name t f => GSetField name t (M1 D m f) where
  gsetField v (M1 x) = M1 (gsetField @name v x)

instance GSetField name t f => GSetField name t (M1 C m f) where
  gsetField v (M1 x) = M1 (gsetField @name v x)

instance t ~ u => GSetField name t (M1 S ('MetaSel ('Just name) su ss ds) (K1 i u)) where
  gsetField v _ = M1 (K1 v)

instance GSetProduct (HasSel name f) name t f g => GSetField name t (f :*: g) where
  gsetField = gsetProduct @(HasSel name f) @name

-- | @a@ has a field called @name@. If not you get a readable type error.
type family HasNamedField (name :: Symbol) a :: Constraint where
  HasNamedField name a = FieldFound (HasSel name (Rep a)) name a

type family FieldFound (found :: Bool) (name :: Symbol) a :: Constraint where
  FieldFound 'True  name a = ()
  FieldFound 'False name a = TypeError
    ('Text "datomlite: " ':<>: 'ShowType a ':<>: 'Text " has no field " ':<>: 'ShowType name)

-- Whether a generic representation (or which side of a product) holds the field
type family HasSel (name :: Symbol) (f :: Type -> Type) :: Bool where
  HasSel name (M1 S ('MetaSel ('Just name) su ss ds) f) = 'True
  HasSel name (M1 D m f) = HasSel name f
  HasSel name (M1 C m f) = HasSel name f
  HasSel name (f :*: g)  = HasSel name f || HasSel name g
  HasSel name f          = 'False

class GSetProduct (left :: Bool) (name :: Symbol) t f g where
  gsetProduct :: t -> (f :*: g) p -> (f :*: g) p

instance GSetField name t f => GSetProduct 'True name t f g where
  gsetProduct v (x :*: y) = gsetField @name v x :*: y

instance GSetField name t g => GSetProduct 'False name t f g where
  gsetProduct v (x :*: y) = x :*: gsetField @name v y

-- | Retract every fact an entity holds. It drops out of reads and queries,
-- and 'history' keeps what it was:
--
-- @
-- commit_ db $ retract empRef
-- commit_ db $ retract (ref \@Employee \"m\@x.io\")
-- @
--
-- An entity with no facts has nothing to retract. A 'ref' nothing holds fails
-- with 'UnknownIdentity'.
retract :: Ref a -> Tx ()
retract r = Tx $ \s -> do
  e <- target r (txView s)
  let facts = [ (e, a, v, Retract) | (a, vs) <- Map.toList (entityAttrs e (txView s)), v <- vs ]
  pure ((), emit facts [] s)

-- | The database as this transaction sees it so far. Query it to decide what
-- to write next:
--
-- @
-- commit db $ do
--   Just acct <- load acctRef \<$\> txDb
--   when (acct.balance < amount) $ abort \"insufficient funds\"
--   update acctRef (\\a -> a { balance = a.balance - amount })
-- @
txDb :: Tx Db
txDb = Tx $ \s -> Right (txView s, s)

-- | Give up: the commit fails with 'Aborted' and nothing is written.
abort :: Text -> Tx a
abort msg = Tx $ \_ -> Left (Aborted msg)

-- | One write, for committing a list of them:
--
-- @
-- commit_ db
--   [ Upsert (Department \"ENG\" \"Engineering\")
--   , Upsert (Employee \"m\@x.io\" \"Max\" (ref \@Department \"ENG\") ...)
--   , Delete (ref \@Employee \"old\@x.io\")
--   ]
-- @
--
-- They run in order as one transaction, each seeing the ones before it.
data Write where
  -- | 'insert' the record as a new entity.
  Insert :: Entity a => a -> Write
  -- | 'upsert' it: update the entity holding one of its identities, or insert.
  Upsert :: Entity a => a -> Write
  -- | 'update' an entity with a function on its record.
  Update :: Entity a => Ref a -> (a -> a) -> Write
  -- | 'retract' every fact the entity holds ('history' keeps them).
  Delete :: Ref a -> Write

-- | What 'commit' takes: a 'Tx' do-block, or a list of 'Write's.
class Writes w where
  -- | What committing it hands back: the 'Tx' result, or @()@ for a list.
  type Result w
  toTx :: w -> Tx (Result w)

instance Writes (Tx a) where
  type Result (Tx a) = a
  toTx = id

-- For any list so an empty one (@commit db []@) needs no annotation
instance w ~ Write => Writes [w] where
  type Result [w] = ()
  toTx = traverse_ step
    where
      step (Insert a)   = insert_ a
      step (Upsert a)   = upsert_ a
      step (Update r f) = update r f
      step (Delete r)   = retract r

-- | Commit a list of 'Write's or a 'Tx', all or nothing, and check 'Identity'
-- uniqueness. Fails with 'UniqueViolation' if two entities would hold the
-- same identity value (whether the other was already there or made in this
-- transaction). Nothing is written on failure.
--
-- The 'Db' comes first so the writes can go last: @commit db [...]@ or
-- @commit db $ do ...@.
--
-- Strict: a value that throws does so here... so a 'Right' never holds a 'Db'
-- that blows up later.
commit :: Writes w => Db -> w -> Either TxError (Result w, Db)
commit = commitWith id

-- | 'commit' with a wall-clock time on the transaction, for 'asOfTime' and
-- 'trTime'. It's logged as a @db\/txInstant@ datom so it survives a save and
-- restore. A time before the last stamp is moved up to it.
--
-- 'transact' stamps with the clock. This is for tests or a clock of your own.
commitAt :: Writes w => UTCTime -> Db -> w -> Either TxError (Result w, Db)
commitAt t = commitWith (stampTime t)

commitWith :: Writes w => (Db -> Db) -> Db -> w -> Either TxError (Result w, Db)
commitWith finish db w = do
  (a, s) <- runTx (toTx w) (TxState db [])
  let db' = closeTx (finish (txView s))
  checkUnique (txClaims s) db'
  db' `seq` pure (a, db')

-- | 'commit' but keep only the new database.
commit_ :: Writes w => Db -> w -> Either TxError Db
commit_ db = fmap snd . commit db

-- Fail if a claimed identity value ends up on more than one entity. We check
-- the result and not each step... so a transaction can move one between
-- entities or retract and reuse it
checkUnique :: [(Attr, Value)] -> Db -> Either TxError ()
checkUnique claims db = mapM_ unique claims
  where
    unique (a, v) = case entitiesWith a v db of
      es@(_:_:_) -> Left (UniqueViolation a v es)
      _          -> Right ()

-- | 'commit' on a connection: the same writes, the same result and new 'Db'
-- back, and the connection now holds that 'Db'. A 'TxError' or an exception
-- leaves the connection as it was.
--
-- @
-- conn <- connect emptyDb
-- Right (empRef, db) <- transact conn $ do
--   eng <- insert (Department \"ENG\" \"Engineering\")
--   insert (Employee \"m\@x.io\" \"Max\" eng 120000 mempty Nothing)
-- @
--
-- The 'Db' is the one this commit made, even if another thread has committed
-- since. 'snapshot' reads whatever the connection holds now.
--
-- The transaction is stamped with the current time (see 'commitAt').
transact :: Writes w => Conn -> w -> IO (Either TxError (Result w, Db))
transact conn w = do
  now <- getCurrentTime
  atomicUpdate conn $ \db -> case commitAt now db w of
    Left err       -> (db,  Left err)
    Right (a, db') -> (db', Right (a, db'))

--------------------------------------------------------------------------------
-- Reading records

-- | A record read from the database along with its 'Ref'. Every read hands
-- one back ('load', 'every', 'find', 'match', 'follow'). Read its fields with
-- @p.name@, its ref with 'refOf' and the plain record with 'recordOf'.
data Stored a = Stored (Ref a) a
  deriving (Eq, Show)

-- | The 'Ref' of a stored record. Always the entity's id, never a lookup ref,
-- so it's what to compare with a ref field: @guard (e.dept == refOf d)@.
refOf :: Stored a -> Ref a
refOf (Stored r _) = r

-- | The plain record.
recordOf :: Stored a -> a
recordOf (Stored _ a) = a

-- | @p.field@ is the record's own field.
instance HasField name a t => HasField (name :: Symbol) (Stored a) t where
  getField (Stored _ a) = getField @name a

-- | Read a record through its 'Ref' (a lookup ref works too). Its own ref
-- fields stay 'Ref's:
--
-- @
-- load empRef db   -- :: Maybe (Stored Employee)
-- @
--
-- 'Nothing' means no facts of this type. Either it was retracted or never
-- written (a record of only 'Nothing' and empty fields writes no datoms). An
-- entity that is there but doesn't decode throws a 'LoadError'... except in a
-- 'since' view which just skips it.
load :: forall a. Entity a => Ref a -> Db -> Maybe (Stored a)
load r db = do
  e <- resolve r db
  Stored (Ref e) <$> loadRecord e db

-- | Every entity of a type (in creation order). Throws like 'load' on one
-- that doesn't decode.
--
-- @
-- [ p.name | p <- every \@Person db ]
-- @
every :: forall a. Entity a => Db -> [Stored a]
every db =
  [ Stored (Ref e) a | e <- entitiesInNamespace (ename (Proxy @a)) db
                     , Just a <- [loadRecord e db] ]

-- | The entity holding an 'Identity' value:
--
-- @
-- find \@Person \"m\@x.io\" db   -- :: Maybe (Stored Person)
-- @
--
-- Checked like 'ref': the value has the field's type, and a record with
-- several identities says which with 'findBy'.
find
  :: forall a name t. (name ~ SoleIdentity a (Identity a), ByIdentity a name t)
  => FieldValue t -> Db -> Maybe (Stored a)
find = findBy @a @name

-- | 'find' by a named 'Identity' field: @findBy \@User \@\"handle\" \"max\" db@.
findBy :: forall a name t. ByIdentity a name t => FieldValue t -> Db -> Maybe (Stored a)
findBy v = load (refBy @a @name v)

-- | A lookup ref: the entity holding an 'Identity' value. Put one in a record
-- and commit finds the entity, even one written earlier in the same commit:
--
-- @
-- commit_ db
--   [ Upsert (Department \"ENG\" \"Engineering\")
--   , Upsert (Employee \"m\@x.io\" \"Max\" (ref \@Department \"ENG\") ...)
--   ]
-- @
--
-- 'load', 'follow', 'update' and 'retract' take one too. If no entity holds
-- the value, a write fails with 'UnknownIdentity' and a read finds nothing.
-- The value has the field's type ('Maybe' and 'Set' fields take the value
-- inside). A record with several identities says which with 'refBy'.
ref
  :: forall a name t. (name ~ SoleIdentity a (Identity a), ByIdentity a name t)
  => FieldValue t -> Ref a
ref = refBy @a @name

-- | 'ref' by a named 'Identity' field: @refBy \@User \@\"handle\" \"max\"@.
refBy :: forall a name t. ByIdentity a name t => FieldValue t -> Ref a
refBy v = LookupRef (attrName (ename (Proxy @a)) (symbolVal (Proxy @name))) (toValue v)

-- What 'ref', 'find' and their @By@ versions need: @name@ is one of @a@'s
-- identities and @t@ its field's type
type ByIdentity a name t =
  ( Entity a, IdentityNamed name a (Identity a), KnownSymbol name
  , HasField name a t, ToValue (FieldValue t) )

-- | What a field takes as a value to look up or match: the value inside a
-- 'Maybe' or 'Set'.
type family FieldValue t where
  FieldValue (Maybe a) = a
  FieldValue (Set a)   = a
  FieldValue t         = t

-- The one 'Identity' of a record
type family SoleIdentity a (ids :: [Symbol]) :: Symbol where
  SoleIdentity a '[n] = n
  SoleIdentity a '[]  = TypeError
    ('Text "datomlite: " ':<>: 'ShowType a ':<>: 'Text " has no Identity to look up by"
       ':$$: 'Text "Name one in its Entity instance, or use matchBy")
  SoleIdentity a ids  = TypeError
    ('Text "datomlite: " ':<>: 'ShowType a ':<>: 'Text " has several identities, "
       ':<>: 'ShowType ids
       ':$$: 'Text "Say which one: findBy @" ':<>: 'ShowType a ':<>: 'Text " @\"field\" (or refBy)")

type family IdentityNamed (name :: Symbol) a (ids :: [Symbol]) :: Constraint where
  IdentityNamed name a (name ': ids) = ()
  IdentityNamed name a (n ': ids)    = IdentityNamed name a ids
  IdentityNamed name a '[]           = TypeError
    ('Text "datomlite: " ':<>: 'ShowType name ':<>: 'Text " isn't an Identity of "
       ':<>: 'ShowType a
       ':$$: 'Text "Match on it with matchBy, or add it to Identity " ':<>: 'ShowType a)

-- An entity's record of type @a@, if it has facts of that type
loadRecord :: forall a. Entity a => EntityId -> Db -> Maybe a
loadRecord e db
  | Map.null own = Nothing
  | otherwise    = case reconstruct e own of
      Right a -> Just a
      Left err
        | partialView db -> Nothing
        | otherwise      -> throw err
  where
    prefix = ename (Proxy @a) <> "/"
    own    = Map.filterWithKey (\k _ -> prefix `T.isPrefixOf` k) (entityAttrs e db)

-- | A stored entity that doesn't decode as the record asked for. Thrown by
-- reads, since it means the data and the record type have drifted apart.
data LoadError
  = MissingField Attr EntityId
  | BadValue Attr [Value] EntityId
  deriving (Eq, Show)

instance Exception LoadError where
  displayException (MissingField a (EntityId e)) =
    "datomlite: entity " <> show e <> " has no value for required field "
      <> T.unpack a <> " (added after it was written? make it a Maybe)"
  displayException (BadValue a vs (EntityId e)) =
    "datomlite: entity " <> show e <> " holds " <> show vs <> " for "
      <> T.unpack a <> ", which doesn't decode as the field's type"

--------------------------------------------------------------------------------
-- Queries

-- Queries are a list monad over a fixed 'Db'. 'match' scans, 'guard'
-- filters, and 'follow' and 'matchBy' look the entity up directly.

-- | Each bind ranges over candidates and 'Control.Monad.guard' prunes them.
-- A failed pattern match (@Just n <- pure e.nick@) drops the candidate too.
newtype Query a = Query { runQ :: Db -> [a] }

instance Functor Query where
  fmap f (Query g) = Query (map f . g)

instance Applicative Query where
  pure x = Query (const [x])
  (<*>)  = ap

instance Monad Query where
  Query g >>= k = Query (\db -> concat [ runQ (k x) db | x <- g db ])

instance MonadFail Query where
  fail _ = empty

instance Alternative Query where
  empty = Query (const [])
  Query f <|> Query g = Query (\db -> f db ++ g db)

instance MonadPlus Query

-- | Run a query against any 'Db'... @query (asOf t db)@ asks the past.
query :: Db -> Query a -> [a]
query db (Query f) = f db

-- | Range over every entity of a type ('every' inside a query).
match :: forall a. Entity a => Query (Stored a)
match = Query (every @a)

-- | Like 'match' but only the entities whose field holds a value. Goes through
-- the index:
--
-- @
-- e <- matchBy \@Employee \@\"email\" \"m\@x.io\" -- an identity
-- e <- matchBy \@Employee \@\"dept\" (refOf d) -- a reverse ref
-- e <- matchBy \@Employee \@\"skills\" haskell -- one of many
-- e <- matchBy \@Employee \@\"dept\" (ref \@Department \"ENG\")
-- @
--
-- The field and value type are checked against the record. For a 'Maybe' or
-- 'Set' field just pass the value inside.
matchBy
  :: forall a name t
   . ( Entity a, HasNamedField name a, KnownSymbol name, HasField name a t
     , ToValue (FieldValue t) )
  => FieldValue t -> Query (Stored a)
matchBy v = Query $ \db ->
  [ s | w <- resolved db (toValue v)
      , e <- entitiesWith attr w db
      , Just s <- [load (Ref e) db] ]
  where
    attr = attrName (ename (Proxy @a)) (symbolVal (Proxy @name))
    -- a lookup ref matches the entity it names, or nothing
    resolved db (VLookup a w) = maybe [] (pure . VRef) (uniqueAt a w db)
    resolved _  w             = [w]

-- | Walk a 'Ref' to its entity (or to nothing if it was retracted). The cheap
-- join: @d <- follow e.dept@. A lookup ref works too:
-- @d <- follow (ref \@Department \"ENG\")@.
follow :: Entity a => Ref a -> Query (Stored a)
follow r = Query $ \db -> maybe [] pure (load r db)

-- | Range over a 'Data.Set.Set' or 'Maybe' field: @s <- oneOf e.skills@.
oneOf :: Foldable f => f a -> Query a
oneOf xs = Query (const (toList xs))

-- | Everything reachable from @x@ in one or more steps, each entity once. The
-- fixed point of a recursive rule... so it's fine on cycles where plain
-- recursion would loop forever:
--
-- @
-- next p = do
--   r <- matchBy \@Road \@\"from\" (refOf p)
--   follow r.to
--
-- p <- closure next paris   -- paris too if a road loops back
-- @
--
-- Breadth first, nearest first. Lazy so 'exists' stops at the first hit.
closure :: (Stored a -> Query (Stored a)) -> Stored a -> Query (Stored a)
closure step x = Query $ \db ->
  let next v = Seq.fromList (runQ (step v) db)
      go _ Empty = []
      go seen (v :<| queue)
        | refOf v `Set.member` seen = go seen queue
        | otherwise                 = v : go (Set.insert (refOf v) seen) (queue >< next v)
  in go Set.empty (next x)

-- | Keep the row if the subquery has a result (a semi-join). It runs against
-- the same 'Db' as the query around it:
--
-- @
-- o <- match \@Order
-- exists (matchBy \@Employee \@\"email\" o.buyer)
-- @
exists :: Query a -> Query ()
exists (Query f) = Query $ \db -> [ () | not (null (f db)) ]

-- | Keep the row if the subquery has no result (an anti-join):
--
-- @
-- e <- match \@Employee
-- none (matchBy \@Order \@\"buyer\" e.email)
-- @
none :: Query a -> Query ()
none (Query f) = Query $ \db -> [ () | null (f db) ]

--------------------------------------------------------------------------------
-- Window functions

-- | A value for each row of a sorted group. Rows are paired with their sort
-- key @o@.
--
-- An 'Applicative' so several run over the same groups in one go:
--
-- @
-- (,) \<$\> rank \<*\> whole (sum . map salary)
-- @
newtype Window o r b = Window ([(o, r)] -> [b])

instance Functor (Window o r) where
  fmap f (Window g) = Window (map f . g)

instance Applicative (Window o r) where
  pure x = Window (map (const x))
  Window f <*> Window g = Window (\rs -> zipWith ($) (f rs) (g rs))

-- | Group rows by @part@, sort each group by @ord@ and pair every row with
-- what @w@ gives it. SQL's @w OVER (PARTITION BY part ORDER BY ord)@:
--
-- @
-- window dept (Down . salary) rank rows
-- @
--
-- Groups come out in @part@ order and ties keep the order they came in.
-- @const ()@ makes one group of everything.
window :: (Ord p, Ord o) => (r -> p) -> (r -> o) -> Window o r b -> [r] -> [(r, b)]
window part ord (Window w) rows = concat [ zip (map snd g) (w g) | g <- Map.elems groups ]
  where
    -- fromListWith conses, so reverse to put each group back in input order
    groups = Map.map (sortBy (comparing fst) . reverse)
               (Map.fromListWith (++) [ (part r, [(ord r, r)]) | r <- rows ])

-- | 1, 2, 3... ties get different numbers.
rowNumber :: Window o r Int
rowNumber = Window (zipWith const [1 ..])

-- | Ties share a rank and the next one skips: 1, 1, 3.
rank :: Eq o => Window o r Int
rank = Window $ \g ->
  let sizes = map length (group (map fst g))
  in concat (zipWith replicate sizes (scanl (+) 1 sizes))

-- | Ties share a rank and the next one doesn't skip: 1, 1, 2.
denseRank :: Eq o => Window o r Int
denseRank = Window $ \g ->
  concat (zipWith replicate (map length (group (map fst g))) [1 ..])

-- | @f@ of the row @n@ before this one in its group. 'Nothing' near the start.
lag :: Int -> (r -> a) -> Window o r (Maybe a)
lag n f = Window $ \g -> take (length g) (replicate n Nothing ++ map (Just . f . snd) g)

-- | @f@ of the row @n@ after this one in its group. 'Nothing' near the end.
lead :: Int -> (r -> a) -> Window o r (Maybe a)
lead n f = Window $ \g -> take (length g) (drop n (map (Just . f . snd) g) ++ repeat Nothing)

-- | @f@ of the whole group, on every row of it: @whole (sum . map salary)@.
whole :: ([r] -> a) -> Window o r a
whole f = Window $ \g -> let x = f (map snd g) in map (const x) g

-- | @f@ of the group up to and including this row: @running (sum . map salary)@.
running :: ([r] -> a) -> Window o r a
running f = Window $ \g -> map f (drop 1 (inits (map snd g)))

--------------------------------------------------------------------------------
-- Reacting to commits

-- A subscriber is a thread that blocks until the basis moves, then catches up
-- from the log... Writers never wait on subscribers and a slow callback only
-- holds up its own thread.

-- | Call @f@ with every commit from now on, in tx order.
--
-- One 'TxReport' per commit even when several land at once. @f@ runs on its
-- own thread so it can read or commit. If it throws the error goes to stderr
-- and the listener keeps going.
--
-- Returns the action that stops it. Safe to call twice or from inside @f@.
--
-- @
-- stop <- listen conn $ \\r -> putStrLn (show (length (trDatoms r)) <> " datoms")
-- @
listen :: Conn -> (TxReport -> IO ()) -> IO (IO ())
listen conn f = subscribe conn (const []) (\seen db -> map f (txsAfter seen db))

-- | A live query. Calls @f@ with @q@'s result now... then again whenever it
-- changes.
--
-- @q@ runs on the latest 'Db' so if commits outpace @f@ it just skips to the
-- current answer. Errors are handled as in 'listen'.
--
-- @
-- stop <- watch conn (length . every \@Employee) $ \\n -> putStrLn (show n <> " employees")
-- @
watch :: Eq a => Conn -> (Db -> a) -> (a -> IO ()) -> IO (IO ())
watch conn q f = do
  lastSeen <- newIORef Nothing   -- only the watch's own thread touches it
  let notify db = do
        r    <- evaluate (q db)
        prev <- readIORef lastSeen
        when (prev /= Just r) $ writeIORef lastSeen (Just r) >> f r
  subscribe conn (\db -> [notify db]) (\_ db -> [notify db])

-- Fork a thread that runs @start@ on the current 'Db', then @step@ on each
-- newer one... until stopped. The start is read before returning, so no later
-- commit is missed
subscribe :: Conn -> (Db -> [IO ()]) -> (TxId -> Db -> [IO ()]) -> IO (IO ())
subscribe conn start step = do
  stop <- newTVarIO False
  db0  <- snapshot conn
  let deliver = mapM_ $ \act -> do
        stopped <- readTVarIO stop
        unless stopped (guarded act)
      loop seen = do
        next <- atomically $ (Nothing <$ (readTVar stop >>= check))
                    `orElse` (Just <$> awaitAfter conn seen)
        forM_ next $ \db -> deliver (step seen db) >> loop (basisT db)
  _ <- forkIO (deliver (start db0) >> loop (basisT db0))
  pure (atomically (writeTVar stop True))

-- Report and swallow a callback's exception. Async exceptions still end the
-- thread. One write... so it can't interleave with other output on stderr
guarded :: IO () -> IO ()
guarded act = act `catch` \e ->
  if isAsync e then throwIO e
  else BS.hPut stderr . T.encodeUtf8 . T.pack $
         "datomlite: subscriber threw: " <> displayException e <> "\n"
  where
    isAsync :: SomeException -> Bool
    isAsync e = isJust (fromException e :: Maybe SomeAsyncException)
