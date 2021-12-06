{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module ListUtils
  (
  elemCount
  , columns
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
