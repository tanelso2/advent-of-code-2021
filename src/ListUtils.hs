module ListUtils
  (
  elemCount
  , columns
  ) where

import qualified Data.Map as M

elemCount :: (Foldable f, Ord a) => f a -> M.Map a Int
elemCount = foldr (M.alter inc) M.empty
  where
    inc Nothing = Just 1
    inc (Just x) = Just $ x+1

columns :: [[a]] -> [[a]]
columns xs = foldr1 f $ map (map (:[])) xs
  where
    f line acc = zipWith (++) line acc
