{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module ListUtils
  (
  elemCount
  , columns
  , mapi
  ) where

import Data.Hashable

import GenericMap

elemCount :: (Foldable f, Ord a, Hashable a, Map m a Int) => f a -> m
elemCount = foldr (alter inc) empty
  where
    inc Nothing = Just 1
    inc (Just x) = Just $ x+1

columns :: [[a]] -> [[a]]
columns xs = foldr1 f $ map (map (:[])) xs
  where
    f line acc = zipWith (++) line acc

mapi :: (a -> Int -> b) -> [a] -> [b]
mapi f as = map f' $ zip as $ [0..]
  where f' (v,i) = f v i
