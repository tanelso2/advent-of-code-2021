module ListUtils
  (
  elemCount
  , columns
  ) where

import Data.Hashable
import qualified Data.HashMap.Strict as M

elemCount :: (Foldable f, Ord a, Hashable a) => f a -> M.HashMap a Int
elemCount = foldr (M.alter inc) M.empty
  where
    inc Nothing = Just 1
    inc (Just x) = Just $ x+1

columns :: [[a]] -> [[a]]
columns xs = foldr1 f $ map (map (:[])) xs
  where
    f line acc = zipWith (++) line acc
