module Day14
  (
  doDay14
  ) where

import Lib
import ListUtils

import Text.ParserCombinators.Parsec

import Data.List
import qualified Data.Map as M

type Pair = (Char, Char)
type ConTuple = (Pair, Char)
type ConMap = M.Map Pair Char
type PairCount = M.Map Pair Int

type Polymer = (Char, PairCount, Char)

type Day14Input = ([ConTuple], String)

parseConTuple :: Parser ConTuple
parseConTuple = do
  a <- letter
  b <- letter
  string " -> "
  c <- letter
  optional newline
  return ((a,b),c)

parseDay14 :: Parser Day14Input
parseDay14 = do
  initialState <- many letter
  optional $ many newline
  cs <- many1 parseConTuple
  return (cs, initialState)

parseInput :: String -> Either ParseError Day14Input
parseInput = parse parseDay14 ""

doDay14 :: IO ()
doDay14 = doDay 14 parseInput part1 part2

mkPolymer :: String -> Polymer
mkPolymer s = (head s, pairMap, last s)
  where
    pairs = zip s (tail s)
    pairMap = elemCount pairs

generatePairs :: ConMap -> Pair -> (Pair, Pair)
generatePairs cm (a,b) = ((a,c),(c,b))
  where maybeC = (M.!?) cm (a,b)
        c = f maybeC
        f Nothing = error $ "Failed on " ++ show cm
        f (Just x) = x

mapPlus :: (Ord k) => Int -> k -> M.Map k Int -> M.Map k Int
mapPlus v = M.alter f
  where f Nothing = Just v
        f (Just x) = Just (x+v)

iteratePairCount :: ConMap -> PairCount -> PairCount
iteratePairCount cm pc = M.foldrWithKey f M.empty pc
  where
    f (a,b) v m = mapPlus v p1 $ mapPlus v p2 m
      where (p1,p2) = generatePairs cm (a,b)

elemCountPolymer :: Polymer -> [(Char, Int)]
elemCountPolymer (fl, pc, ll) = z
  where x = M.foldrWithKey (\(a,b) v m -> mapPlus v a $ mapPlus v b m) M.empty pc
        y = map f' $ map f $ M.toList x
        f (c,v) = (c, v `div` 2)
        -- if c is the first letter or last letter, then we just deleted an extra one
        -- that didn't actually exist. Add it back now
        f' (c,v) = (c, if c == fl || c == ll then v + 1 else v)
        z = sortOn snd y

part1 :: Day14Input -> Result Int
part1 (cs,s) = Verified 3118 $ res
  where
    (firstLetter, pc, lastLetter) = mkPolymer s
    cm = M.fromList cs
    gens = iterate (iteratePairCount cm) pc
    tenthGen = head $ drop 10 gens
    charCount = elemCountPolymer (firstLetter,tenthGen,lastLetter)
    (_,m1) = head charCount
    (_,m2) = last charCount
    res = m2 - m1

part2 :: Day14Input -> Result Int
part2 (cs,s) = Verified 4332887448171 $ res
  where
    (firstLetter, pc, lastLetter) = mkPolymer s
    cm = M.fromList cs
    gens = iterate (iteratePairCount cm) pc
    genX = head $ drop 40 gens
    charCount = elemCountPolymer (firstLetter,genX,lastLetter)
    (_,m1) = head charCount
    (_,m2) = last charCount
    res = m2 - m1
