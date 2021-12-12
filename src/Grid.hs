module Grid
  (
  Grid
  , grid
  , unGrid
  , mapi
  , alter
  , alterBy
  , neighbors
  , neighbors'
  , neighborsBy
  , (!?)
  , allpoints
  ) where

import Data.Foldable
import Data.Functor

import qualified ListUtils (mapi)

newtype Grid a = G [[a]]

grid :: [[a]] -> Grid a
grid x = G x

unGrid :: Grid a -> [[a]]
unGrid (G x) = x

instance Foldable Grid where
  foldr f acc g =
    foldr (\row acc' -> foldr f acc' row) acc $ unGrid g

instance Functor Grid where
  fmap f g = G $ fmap (fmap f) $ unGrid g

instance Traversable Grid where
-- traverse :: (a -> f b) -> Grid a -> f (Grid b)
-- traverse :: (a -> f b) -> [a] -> f [b]
-- sequenceA :: [f a] -> f [a]
  traverse f (G g) = fmap G res
    where res = sequenceA $ map (traverse f) g

type Index = (Int,Int)

mapi :: (a -> Index -> b) -> Grid a -> Grid b
mapi f (G g) = G $ ListUtils.mapi (\r x -> ListUtils.mapi (\v y -> f v (x,y)) r) g

alter :: (a -> a) -> Index -> Grid a -> Grid a
alter f (x,y) g = mapi f' g
  where
    f' v (i,j) = if (i,j) == (x,y)
                 then f v
                 else v

alterBy :: (a -> Index -> Bool) -> (a -> a) -> Grid a -> Grid a
alterBy predFn f g = mapi f' g
  where f' v (i,j) = if predFn v (i,j)
                     then f v
                     else v

neighbors :: Grid a -> Index -> [Index]
neighbors = neighborsBy diagonals

neighbors' :: Grid a -> Index -> [Index]
neighbors' = neighborsBy cartesian

neighborsBy :: (Index -> [Index]) -> Grid a -> Index -> [Index]
neighborsBy candidateFun g (x,y) = filter (inBounds g) candidates
  where
    candidates = candidateFun (x,y)

cartesian :: (Int,Int) -> [(Int,Int)]
cartesian (x,y) = [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]

diagonals :: (Int,Int) -> [(Int,Int)]
diagonals (x,y) = do
  x' <- [x,x+1,x-1]
  y' <- [y,y+1,y-1]
  filter (/=(x,y)) $ return (x',y')

inBounds :: Grid a -> Index -> Bool
inBounds (G g) (x,y) = x < w && x >= 0 && y < h && y >= 0
  where w = length g
        h = length $ head g

bounds :: Grid a -> (Int,Int)
bounds (G g) = (w,h)
  where w = length g
        h = length $ head g

(!?) :: Grid a -> Index -> Maybe a
(G g) !? (x,y) = if inBounds (G g) (x,y)
                 then Just (g !! x !! y)
                 else Nothing

allpoints :: Grid a -> [Index]
allpoints g = do
  let (w,h) = bounds g
  x <- init $ [0..w]
  y <- init $ [0..h]
  return (x,y)
