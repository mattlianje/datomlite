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
  ) where

import  Control.Applicative (Alternative (..))
import  Control.Monad       (MonadPlus, ap)
import  Data.Foldable       (toList)
import  Data.Proxy          (Proxy (..))
import  Data.Sequence       (Seq (..), (><))
import qualified Data.Sequence as Seq
import qualified Data.Set   as Set
import  GHC.Records         (HasField)
import  GHC.TypeLits        (KnownSymbol, symbolVal)

import  Datomlite.Core
import  Datomlite.Schema

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
-- e <- matchBy \@Employee \@\"email\" \"m\@x.io\"   -- an identity
-- e <- matchBy \@Employee \@\"dept\" (refOf d)     -- a reverse ref
-- e <- matchBy \@Employee \@\"skills\" haskell     -- one of many
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
  [ s | w <- resolved db (toValue v), e <- entitiesWith attr w db, Just s <- [load (Ref e) db] ]
  where
    attr = attrName (ename (Proxy @a)) (symbolVal (Proxy @name))
    -- a lookup ref matches the entity it names (or nothing)
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
