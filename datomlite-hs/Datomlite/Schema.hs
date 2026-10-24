{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RoleAnnotations       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

-- Records as the schema. Field types give cardinality and 'Identity' names
-- the unique fields... a Generic-derived 'Entity' shreds a record into datoms
-- and back
module Datomlite.Schema
  ( -- * Field wrappers
    Ref (..)
  , refId
  , ref
  , refBy
  , HasNamedField
  , FieldValue

    -- * Scalar (de)serialisation
  , ToValue (..)
  , FromValue (..)

    -- * Field shapes
  , Shape (..)
  , Field (..)

    -- * Entities
  , Entity (..)
  , entityFields
  , attrName

    -- * Building transactions
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

    -- * A list of writes
  , Write (..)
  , Writes (..)

    -- * Committing
  , TxError (..)
  , commit
  , commit_
  , commitAt
  , transact

    -- * Reading records
  , Stored (..)
  , refOf
  , recordOf
  , load
  , every
  , find
  , findBy
  , LoadError (..)
  ) where

import           Control.Exception (Exception (..), throw)
import           Control.Monad  (ap, void)
import           Data.Foldable  (traverse_)
import           Data.ByteString (ByteString)
import           Data.Kind      (Constraint, Type)
import           Data.List      (nub)
import           Data.Proxy     (Proxy (..))
import qualified Data.Map.Strict as Map
import           Data.Set       (Set)
import qualified Data.Set       as Set
import           Data.Text      (Text)
import qualified Data.Text      as T
import           Data.Time      (Day, UTCTime (..), getCurrentTime)
import           Data.Type.Bool (If, type (&&), type (||))
import           GHC.Generics
import           GHC.Records    (HasField (..))
import           GHC.TypeLits   (ErrorMessage (..), KnownSymbol, Symbol, TypeError,
                                 symbolVal)

import           Datomlite.Core hiding (retract, transact)
import qualified Datomlite.Core as Core

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
  genumFrom t      = maybe (R1 <$> genumFrom t) (Just . L1) (genumFrom t)

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

-- | The attribute names a record is stored under:
-- @["Department/code", "Department/deptName"]@.
entityFields :: Entity a => a -> [Attr]
entityFields = map (\(a, _, _) -> a) . decompose

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
  gdecompose n s@(M1 (K1 x)) =
    [ (attrName n (selName s), shred x, shape (Proxy @a)) ]
  greconstruct n =
    let a = attrName n (selName (undefined :: M1 S s (K1 i a) p))
    in \e m -> let vs = Map.findWithDefault [] a m
               in case unshred vs of
                    Just x  -> Right (M1 (K1 x))
                    Nothing
                      | null vs   -> Left (MissingField a e)
                      | otherwise -> Left (BadValue a vs e)

-- | The attribute for a field: @Type\/field@.
attrName :: Text -> String -> Attr
attrName ty fld = ty <> "/" <> T.pack fld

-- A record's fields with every lookup ref swapped for the entity it names
fieldsIn :: Entity a => Db -> a -> Either TxError [(Attr, [Value], Shape)]
fieldsIn db = traverse resolveField . decompose
  where
    resolveField (attr, vs, sh) = (\vs' -> (attr, nub vs', sh)) <$> traverse resolveValue vs
    resolveValue (VLookup a v) = maybe (Left (UnknownIdentity a v)) (Right . VRef) (uniqueAt a v db)
    resolveValue v             = Right v

-- The entity a ref names, or why there isn't one
target :: Ref a -> Db -> Either TxError EntityId
target (Ref e)         _  = Right e
target (LookupRef a v) db = maybe (Left (UnknownIdentity a v)) Right (uniqueAt a v db)

-- Make entity @e@ match a record's fields: stage only the difference and
-- claim the identity values it newly holds
write :: [Attr] -> EntityId -> [(Attr, [Value], Shape)] -> TxState -> TxState
write ids e fields s = emit ops claims s
  where
    ops = [ (e, attr, v, Retract) | (attr, vs, _) <- fields, v <- current attr, v `notElem` vs ]
       ++ [ (e, attr, v, Assert) | (attr, vs, _) <- fields, v <- vs, v `notElem` current attr ]
    claims = [ (attr, v) | (attr, vs, _) <- fields, attr `elem` ids
                         , v <- vs, v `notElem` current attr ]
    current attr = lookupValues e attr (txView s)

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
      holders = [ e | (attr, vs, _) <- fields, attr `elem` ids
                    , v <- vs, e <- entitiesWith attr v (txView s) ]
  case holders of
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
retract (Ref e) = Tx $ \s ->
  let ds = [ Core.retract e a v | (a, vs) <- Map.toList (entityAttrs e (txView s)), v <- vs ]
  in Right ((), emit ds [] s)
retract r = Tx $ \s -> target r (txView s) >>= \e -> runTx (retract (Ref e)) s

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
checkUnique claims db = mapM_ check claims
  where
    check (a, v) = case entitiesWith a v db of
      es@(_:_:_) -> Left (UniqueViolation a v es)
      _          -> Right ()

-- | 'commit' on a connection: the same writes, the same result and new 'Db'
-- back, and the connection now holds that 'Db'. A 'TxError' or an exception leaves the connection as
-- it was.
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

-- | A record read from the database along with its 'Ref'. Every read hands
-- one back ('load', 'every', 'find', 'Datomlite.match', 'Datomlite.follow').
-- Read its fields with @p.name@, its ref with 'refOf' and the plain record
-- with 'recordOf'.
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
-- 'load', 'Datomlite.follow', 'update' and 'retract' take one too. If no
-- entity holds the value, a write fails with 'UnknownIdentity' and a read
-- finds nothing.
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
