{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RoleAnnotations       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

-- Records as the schema. Field types give cardinality and uniqueness... a
-- Generic-derived 'Entity' shreds a record into datoms and back
module Datomlite.Schema
  ( -- * Field wrappers
    Key (..)
  , Ref (..)
  , refId
  , Unkey
  , HasNamedField

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
  , new
  , save
  , update
  , setField
  , retractEntity
  , txDb
  , abort

    -- * Committing
  , TxError (..)
  , commit
  , commit_
  , commitIO

    -- * Loading records
  , load
  , loadAll
  , LoadError (..)
  ) where

import           Control.Exception (Exception (..), throw)
import           Control.Monad  (ap)
import           Data.ByteString (ByteString)
import           Data.Coerce    (Coercible, coerce)
import           Data.Kind      (Constraint, Type)
import           Data.Proxy     (Proxy (..))
import qualified Data.Map.Strict as Map
import           Data.Set       (Set)
import qualified Data.Set       as Set
import           Data.String    (IsString (..))
import           Data.Text      (Text)
import qualified Data.Text      as T
import           Data.Time      (Day, UTCTime (..))
import           Data.Type.Bool (If, type (&&), type (||))
import           GHC.Generics
import           GHC.Records    (HasField)
import           GHC.TypeLits   (ErrorMessage (..), Symbol, TypeError)

import           Datomlite.Core

-- | Marks a field as the entity's unique identity: @email :: Key Text@.
newtype Key a = Key { unKey :: a }
  deriving (Eq, Ord, Show)

-- | With @OverloadedStrings@ a string literal is a 'Key' too. No need to write
-- @Key@: @Department "ENG" "Engineering"@.
instance IsString a => IsString (Key a) where
  fromString = Key . fromString

-- | Number literals work as 'Key's too: @Widget 42 ...@.
instance Num a => Num (Key a) where
  fromInteger   = Key . fromInteger
  Key a + Key b = Key (a + b)
  Key a - Key b = Key (a - b)
  Key a * Key b = Key (a * b)
  abs    (Key a) = Key (abs a)
  signum (Key a) = Key (signum a)
  negate (Key a) = Key (negate a)

-- | A field's type with 'Key' dropped. This is what @e.field@ and 'setField'
-- see.
type family Unkey t where
  Unkey (Key a) = a
  Unkey t       = t

-- | A typed reference to another entity: @dept :: Ref Department@.
--
-- You only get one from the database ('new' or 'save' or a query) so it
-- always points at the type it claims. Its role is nominal so 'coerce' can't
-- cheat either.
newtype Ref a = Ref EntityId
  deriving (Eq, Ord, Show)
type role Ref nominal

-- | The 'EntityId' behind a 'Ref'.
refId :: Ref a -> EntityId
refId (Ref e) = e

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

instance ToValue (Ref t) where toValue (Ref e) = VRef e
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
  | ShapeUnique    -- ^ exactly one value, unique identity (a 'Key')
  | ShapeOptional  -- ^ zero or one value
  | ShapeMany      -- ^ zero or more values
  deriving (Eq, Show)

-- | How a field type is stored: its 'Shape' and how to turn it into 'Value's
-- and back. Instances cover 'Key', 'Maybe', 'Set' and plain scalars (including
-- 'Ref').
class Field a where
  shape   :: Proxy a -> Shape
  shred   :: a -> [Value]
  unshred :: [Value] -> Maybe a

-- A plain scalar. Overlappable so the instances below win for 'Key', 'Maybe',
-- 'Set' and lists
instance {-# OVERLAPPABLE #-} (ToValue a, FromValue a) => Field a where
  shape _ = ShapeOne
  shred a = [toValue a]
  unshred (v:_) = fromValue v
  unshred []    = Nothing

instance (ToValue a, FromValue a) => Field (Key a) where
  shape _ = ShapeUnique
  shred (Key a) = [toValue a]
  unshred (v:_) = Key <$> fromValue v
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
-- data Employee = Employee { email :: Key Text, name :: Text, ... }
--   deriving (Generic, Entity)
-- @
--
-- or write an empty @instance Entity Employee@.
class Entity a where
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

-- The writes that make entity @e@ match a record, and the key values it newly
-- claims...
diffTo :: Entity a => EntityId -> a -> Db -> (TxData, [(Attr, Value)])
diffTo e a db =
  ( [ (e, attr, v, Retract) | (attr, vs, _) <- fields, v <- current attr, v `notElem` vs ]
    ++ [ (e, attr, v, Assert) | (attr, vs, _) <- fields, v <- vs, v `notElem` current attr ]
  , [ (attr, v) | (attr, vs, ShapeUnique) <- fields, v <- vs, v `notElem` current attr ]
  )
  where
    fields       = decompose a
    current attr = lookupValues e attr db

-- | Why a commit failed. Nothing is written.
data TxError
  = -- | Two entities would hold the same 'Key' value.
    UniqueViolation
      { uvAttr  :: Attr    -- ^ the unique attribute, e.g. @"Employee/email"@
      , uvValue :: Value   -- ^ the value both would hold
      }
    -- | 'update' on an entity with no facts.
  | UnknownEntity EntityId
    -- | The transaction gave up through 'abort' or a failed pattern match.
  | Aborted Text
  deriving (Eq, Show)

-- | A transaction. The 'Ref' from 'new' works right away in later steps and
-- the whole thing commits at once:
--
-- @
-- Right (empRef, db') = 'commit' db $ do
--   eng <- 'new' (Department { code = \"ENG\", deptName = \"Engineering\" })
--   'new' (Employee { email = \"m\@x.io\", dept = eng, ... })
-- @
--
-- Each step sees the ones before it and 'txDb' reads that in-progress
-- database. Small 'Tx's compose into bigger ones (it's a 'Monad' and a
-- 'Monoid' when the result is).
--
-- Under the hood it's the 'Db' being written (every step staged under one
-- pending tx) plus the key claims to check on commit. Ids are handed out as
-- you go... so a tempid is just a real 'EntityId'.
newtype Tx a = Tx { runTx :: TxState -> Either TxError (a, TxState) }

data TxState = TxState
  { txView   :: Db -- the db with this tx's writes so far staged
  , txClaims :: [(Attr, Value)] -- unique-key values that must end up unique
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

-- | Insert a record. The 'Ref' it returns works right away.
new :: Entity a => a -> Tx (Ref a)
new a = Tx $ \s ->
  let (e, view)     = freshEntity (txView s)
      (ops, claims) = diffTo e a view
  in Right (Ref e, emit ops claims s { txView = view })

-- | Upsert by 'Key'. If an entity already holds one of the record's keys it is
-- made to match... otherwise the record is inserted. Either way you get the
-- 'Ref' back:
--
-- @
-- commit db $ save (Department \"ENG\" \"Platform Engineering\")
-- @
--
-- Only the difference is written. A record with no 'Key' is always inserted.
-- Keys pointing at two different entities fail with 'UniqueViolation'.
save :: Entity a => a -> Tx (Ref a)
save a = Tx $ \s ->
  let holders = [ e | (attr, vs, ShapeUnique) <- decompose a, v <- vs
                    , e <- entitiesWith attr v (txView s) ]
  in case holders of
       []    -> runTx (new a) s
       (e:_) -> let (ops, claims) = diffTo e a (txView s)
                in Right (Ref e, emit ops claims s)

-- | Change an entity with a function on its record:
--
-- @
-- commit_ db $ update maxRef (\\e -> e { name = \"Maximilian\" })
-- @
--
-- Only the difference is written so a one-valued field gets replaced (not
-- doubled up). Changed 'Key's are checked for uniqueness. Fails with
-- 'UnknownEntity' if the entity has no facts.
update :: Entity a => Ref a -> (a -> a) -> Tx ()
update r@(Ref e) f = Tx $ \s -> case load r (txView s) of
  Nothing  -> Left (UnknownEntity e)
  Just old -> let (ops, claims) = diffTo e (f old) (txView s)
              in Right ((), emit ops claims s)

-- | Set one field by name. Handy with 'update':
--
-- @
-- commit_ db $ update maxRef (setField \@\"name\" \"Maximilian\")
-- @
--
-- Record-update syntax works too but gets ambiguous when two records share a
-- field name. Here the record type comes from the 'Ref'. Field and type are
-- checked like @e.name@ and a 'Key' field takes the bare value.
setField
  :: forall name a t
   . ( HasNamedField name a, HasField name a t, Coercible (Unkey t) t
     , Generic a, GSetField name t (Rep a) )
  => Unkey t -> a -> a
setField v = to . gsetField @name (coerce v :: t) . from

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

-- | Retract every fact an entity holds. It drops out of 'load', 'loadAll' and
-- queries.
--
-- @
-- commit_ db $ retractEntity empRef
-- @
retractEntity :: Ref a -> Tx ()
retractEntity (Ref e) = Tx $ \s ->
  let ds = [ retract e a v | (a, vs) <- Map.toList (entityAttrs e (txView s)), v <- vs ]
  in Right ((), emit ds [] s)

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

-- | Run a transaction and check 'Key' uniqueness. Fails with 'UniqueViolation'
-- if two entities would hold the same key value (whether the other was
-- already there or made in this transaction). Nothing is written on failure.
--
-- The 'Db' comes first so the do-block can go last: @commit db $ do ...@.
--
-- Strict: a value that throws does so here... so a 'Right' never holds a 'Db'
-- that blows up later.
commit :: Db -> Tx a -> Either TxError (a, Db)
commit db (Tx g) = do
  (a, s) <- g (TxState db [])
  let db' = closeTx (txView s)
  checkUnique (txClaims s) db'
  db' `seq` pure (a, db')

-- | 'commit' but keep only the new database.
commit_ :: Db -> Tx a -> Either TxError Db
commit_ db = fmap snd . commit db

-- Fail if a claimed key value ends up on more than one entity. We check the
-- result and not each step... so a transaction can move a key between
-- entities or retract and reuse it
checkUnique :: [(Attr, Value)] -> Db -> Either TxError ()
checkUnique claims db = mapM_ check claims
  where
    check (a, v)
      | length (entitiesWith a v db) > 1 = Left (UniqueViolation a v)
      | otherwise                        = Right ()

-- | 'commit' on a connection. Swaps in the new 'Db' and returns the
-- transaction's result. A 'TxError' or an exception leaves the connection as
-- it was.
--
-- @
-- conn <- connect emptyDb
-- Right empRef <- commitIO conn $ do
--   eng <- new (Department \"ENG\" \"Engineering\")
--   new (Employee \"m\@x.io\" \"Max\" eng 120000 mempty Nothing)
-- @
commitIO :: Conn -> Tx a -> IO (Either TxError a)
commitIO conn tx = atomicUpdate conn $ \db ->
  case commit db tx of
    Left err       -> (db,  Left err)
    Right (a, db') -> (db', Right a)

-- | Read a record through its 'Ref'. Its own ref fields stay 'Ref's:
--
-- @
-- load empRef db   -- :: Maybe Employee
-- @
--
-- 'Nothing' means no facts of this type. Either it was retracted or never
-- written (a record of only 'Nothing' and empty fields writes no datoms). An
-- entity that is there but doesn't decode throws a 'LoadError'... except in a
-- 'since' view which just skips it.
load :: forall a. Entity a => Ref a -> Db -> Maybe a
load (Ref e) db
  | Map.null own = Nothing
  | otherwise    = case reconstruct e own of
      Right a -> Just a
      Left err
        | partialView db -> Nothing
        | otherwise      -> throw err
  where
    prefix = ename (Proxy @a) <> "/"
    own    = Map.filterWithKey (\k _ -> prefix `T.isPrefixOf` k) (entityAttrs e db)

-- | Every entity of a type with its 'Ref' (in creation order). Throws like
-- 'load' on one that doesn't decode.
loadAll :: forall a. Entity a => Db -> [(Ref a, a)]
loadAll db =
  [ (r, a) | e <- entitiesInNamespace (ename (Proxy @a)) db
           , let r = Ref e, Just a <- [load r db] ]

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
