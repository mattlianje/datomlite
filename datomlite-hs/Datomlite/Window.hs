-- Window functions. A query gives back a plain list so these are list
-- functions too... group the rows, sort each group and give every row a value
-- worked out from its group
module Datomlite.Window
  ( Window
  , window
  , rowNumber
  , rank
  , denseRank
  , lag
  , lead
  , whole
  , running
  ) where

import           Data.List       (group, inits, sortBy)
import qualified Data.Map.Strict as Map
import           Data.Ord        (comparing)

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
    groups = Map.map (sortBy (comparing fst) . reverse)
               (Map.fromListWith (++) [ (part r, [(ord r, r)]) | r <- rows ])

-- | 1, 2, 3... ties get different numbers.
rowNumber :: Window o r Int
rowNumber = Window (zipWith const [1 ..])

-- | Ties share a rank and the next one skips: 1, 1, 3.
rank :: Eq o => Window o r Int
rank = Window $ \g ->
  let ties = group (map fst g)
  in concat (zipWith replicate (map length ties) (scanl (+) 1 (map length ties)))

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
