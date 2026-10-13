{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

-- Queries are a list monad over a fixed 'Db'. 'match' scans and 'guard'
-- filters... 'follow' and 'matchBy' look the entity up directly
module Datomlite.Query
  ( -- * The query monad
    Query
  , query
  , match
  , matchBy
  , follow
  , oneOf
  , closure

    -- * Subqueries
  , exists
  , none

    -- * Matched entities
  , Var
  , refOf
  , recordOf
  ) where

import  Control.Applicative (Alternative (..))
import  Control.Monad       (MonadPlus, ap)
import  Data.Coerce         (Coercible, coerce)
import  Data.Foldable       (toList)
import  Data.Proxy          (Proxy (..))
import  Data.Sequence       (Seq (..), (><))
import qualified Data.Sequence as Seq
import  Data.Set            (Set)
import qualified Data.Set   as Set
import  GHC.Records         (HasField (..))
import  GHC.TypeLits        (KnownSymbol, Symbol, symbolVal)

import  Datomlite.Core
import  Datomlite.Schema

-- | An entity bound in a query. Read its fields with @e.name@ and its 'Ref'
-- with 'refOf'.
data Var a = Var (Ref a) a
  deriving (Eq, Show)

-- | The 'Ref' of a matched entity. Handy to compare with a ref field:
-- @guard (e.dept == refOf d)@.
refOf :: Var a -> Ref a
refOf (Var r _) = r

-- | The whole record of a matched entity.
recordOf :: Var a -> a
recordOf (Var _ a) = a

-- | @e.field@ is the record's own field with 'Key' dropped.
instance
  ( HasField name a t
  , u ~ Unkey t
  , Coercible t u
  ) => HasField (name :: Symbol) (Var a) u where
  getField (Var _ a) = coerce (getField @name a :: t)

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

-- | Range over every entity of a type.
match :: forall a. Entity a => Query (Var a)
match = Query $ \db -> [ Var r a | (r, a) <- loadAll @a db ]

-- | Like 'match' but only the entities whose field holds a value. Goes through
-- the index:
--
-- @
-- e <- matchBy \@Employee \@\"email\" \"m\@x.io\"   -- a Key
-- e <- matchBy \@Employee \@\"dept\" (refOf d)     -- a reverse ref
-- e <- matchBy \@Employee \@\"skills\" haskell     -- one of many
-- @
--
-- The field and value type are checked against the record. For a 'Key',
-- 'Maybe' or 'Set' field just pass the value inside.
matchBy
  :: forall a name t
   . ( Entity a, HasNamedField name a, KnownSymbol name, HasField name a t
     , ToValue (FieldValue t) )
  => FieldValue t -> Query (Var a)
matchBy v = Query $ \db ->
  [ Var r a | e <- entitiesWith attr (toValue v) db, let r = Ref e, Just a <- [load r db] ]
  where attr = attrName (ename (Proxy @a)) (symbolVal (Proxy @name))

-- | What 'matchBy' takes for a field.
type family FieldValue t where
  FieldValue (Key a)   = a
  FieldValue (Maybe a) = a
  FieldValue (Set a)   = a
  FieldValue t         = t

-- | Walk a 'Ref' to its entity (or to nothing if it was retracted). The cheap
-- join: @d <- follow e.dept@.
follow :: Entity a => Ref a -> Query (Var a)
follow r = Query $ \db -> [ Var r a | Just a <- [load r db] ]

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
closure :: (Var a -> Query (Var a)) -> Var a -> Query (Var a)
closure step x = Query $ \db ->
  let go _ Empty = []
      go seen (v :<| vs)
        | refOf v `Set.member` seen = go seen vs
        | otherwise = v : go (Set.insert (refOf v) seen) (vs >< Seq.fromList (runQ (step v) db))
  in go Set.empty (Seq.fromList (runQ (step x) db))

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
