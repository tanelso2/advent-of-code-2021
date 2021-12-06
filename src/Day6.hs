module Day6
  (
  doDay6
  ) where

import Lib
import ParseUtils
import ListUtils

import qualified Data.IntMap.Strict as M

import Data.List
import Text.ParserCombinators.Parsec

parseDay6 :: Parser Day6Input
parseDay6 = sepBy1 int $ char ','


type Day6Input = [Int]

parseInput :: String -> Either ParseError Day6Input
parseInput = parse parseDay6 ""

doDay6 :: IO ()
doDay6 = doDay 6 parseInput part1 part2

nextDay :: Int -> [Int]
nextDay 0  = [6,8]
nextDay n = [n-1]

nextGen :: [Int] -> [Int]
nextGen = concatMap nextDay

type Map = M.IntMap Int

mapPlus :: Int -> Int -> Map -> Map
mapPlus k v m = M.alter f k m
  where f Nothing = Just v
        f (Just x) = Just $ x + v

nextGen' :: Map -> Map
nextGen' old = M.foldrWithKey f M.empty old
  where f k v m
          | k == 0 = mapPlus 6 v $ mapPlus 8 v m
          | otherwise = mapPlus (k-1) v m

-- elemCount = foldr (M.alter inc) M.empty
--   where
--     inc Nothing = Just 1
--     inc (Just x) = Just (x+1)

part1 :: Day6Input -> Result Int
part1 xs = Verified 379414 $ length $ head $ drop 80 gens
  where gens = iterate nextGen xs

part2 :: Day6Input -> Result Int
part2 xs = Verified 1705008653296 $
  foldr1 (+) $ head $ drop 256 gens
  where gens = iterate nextGen' $ elemCount xs
