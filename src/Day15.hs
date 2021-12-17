module Day15
  (
  doDay15
  ) where

import Lib
import Grid

import Text.ParserCombinators.Parsec
import ParseUtils

import Data.Maybe
import qualified Data.Set as S
import qualified Data.Map as M

type Day15Input = Grid Int

parseLine :: Parser [Int]
parseLine = do
  xs <- many1 $ oneOf ['0'..'9']
  optional newline
  let xs' = map (\x -> read (x:[])) xs
  return xs'

parseDay15 = do
  ls <- many parseLine
  return $ grid ls

parseInput :: String -> Either ParseError Day15Input
parseInput = parse parseDay15 ""

doDay15 :: IO ()
doDay15 = doDay 15 parseInput part1 part2

-- HEAP
data Heap p a = HNil | HNode p a [Heap p a] (S.Set a)
  deriving (Eq,Ord,Show)

empty :: Heap p a
empty = HNil

pop :: (Ord p, Ord a) => Heap p a -> (Maybe (p,a), Heap p a)
pop HNil = (Nothing, HNil)
pop (HNode p a hs elems)
  | length hs == 0 = (Just (p,a), HNil)
  | otherwise = (Just (p,a), balance hs)

clean :: (Ord p, Ord a) => [Heap p a] -> [Heap p a]
clean = filter (/=HNil)

balance :: (Eq a, Ord p, Ord a) => [Heap p a] -> Heap p a
balance [] = HNil
balance hs = hs'
  where
    minNode = foldr1 min hs
    hs' = case minNode of
            HNil -> HNil
            HNode p a hs'' elems -> HNode p a (rest++hs'') elems'
              where
                hs''' = rest ++ hs''
                elems' = foldr f (S.singleton a) hs'''
                f HNil s = s
                f (HNode _ _ _ nodeElems) s = S.union nodeElems s
    rest = filter (/=minNode) hs

maxChildren = 8

insert :: (Eq a, Ord p, Ord a) => Heap p a -> p -> a -> Heap p a
insert HNil p a = HNode p a [] (S.singleton a)
insert h@(HNode headP headA hs elems) p a
  | p < headP = HNode p a [h] newElems
  | length hs < maxChildren = HNode headP headA (newNode:hs) newElems
  | otherwise = HNode headP headA [balance (newNode:hs)] newElems
  where newNode = HNode p a [] (S.singleton a)
        newElems = S.insert a elems


member :: (Eq a, Ord p, Ord a) => a -> Heap p a -> Bool
member a HNil = False
member a (HNode _ _ _ s) = S.member a s
-- END HEAP
-- -- MAPS AS HEAPS
--
-- type Heap p a = M.Map a p
--
-- pop :: (Eq a, Ord p, Ord a) => Heap p a -> (Maybe (p,a), Heap p a)
-- pop h
--   | M.null h = (Nothing, h)
--   | otherwise = (Just (minP, minA), M.delete minA h)
--     where
--       (minA, minP) = fromJust $ M.foldrWithKey f Nothing h
--       f a p Nothing = Just (a,p)
--       f a p (Just (minA, minP)) = if p < minP
--                                     then Just $ (a,p)
--                                     else Just $ (minA,minP)
--
-- compPriority :: (Eq a, Ord p, Ord a) => p -> a -> Heap p a -> Heap p a
-- compPriority p a h = M.alter f a h
--   where f Nothing = Just p
--         f (Just x) = if p < x
--                        then Just p
--                        else Just x
--
-- insert :: (Ord a) => Heap p a -> p -> a -> Heap p a
-- insert h p a = M.insert a p h
-- -- END MAPS AS HEAPS

shortestCost :: Heap Int (Int,Int) -> S.Set (Int,Int) -> Grid Int -> Int
shortestCost h visited g =
  case pop h of
    (Nothing, _) -> error "Uhhh... we should have reached the end by now"
    (Just (cost, p), h') ->
      if p == (\(x,y) -> (x-1,y-1)) (bounds g)
      then cost
      else shortestCost h'' s' g
        where
          h'' = foldr f h' ns
          s' = S.insert p visited
          ns = filter (\x -> not (x `elem` visited) && not (member x h')) $ neighbors' g p
          f x h = insert h newCost x
            where newCost = cost + (fromJust $ g !? x)

shortCost :: Grid Int -> Int
shortCost g = shortestCost h (S.singleton startingPos) g
  where h = insert empty 0 startingPos
        startingPos = (0,0)

part1 :: Day15Input -> Result Int
part1 xs = Res $ shortCost xs

mkBiggerGrid :: Grid Int -> Grid Int
mkBiggerGrid g = res
  where
    (w,h) = bounds g
    newG = gridOfSize (5*w) (5*h)
    res = mapi f newG
    f _ (x,y)
      | inBounds g (x,y) = fromJust $ g !? (x,y)
      | otherwise = value
        where (m1, x') = x `divMod` w
              (m2, y') = y `divMod` h
              m = m1 + m2
              origValue = fromJust (g !? (x',y'))
              v = m + origValue
              value = if v > 9 then (v `mod` 10) + 1 else v


part2 :: Day15Input -> Result Int
part2 xs = Res $ shortCost $ mkBiggerGrid xs
