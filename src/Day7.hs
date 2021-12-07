module Day7
  (
  doDay7
  ) where

import Lib
import ParseUtils

import Text.ParserCombinators.Parsec

parseDay7 :: Parser Day7Input
parseDay7 = sepBy1 int $ char ','

type Day7Input = [Int]

parseInput :: String -> Either ParseError Day7Input
parseInput = parse parseDay7 ""

doDay7 :: IO ()
doDay7 = doDay 7 parseInput part1 part2

fuelCost :: [Int] -> Int -> Int
fuelCost crabs dest = sum $ map (distance dest) crabs

part1 :: Day7Input -> Result Int
part1 xs = Verified 359648 $ foldr1 min $ map (fuelCost xs) $ [lb..ub]
  where
    ub = foldr1 max xs
    lb = foldr1 min xs

costForDistance :: Int -> Int
costForDistance n = (n * (n+1)) `div` 2

distance :: Int -> Int -> Int
distance a b = abs $ a - b

fuelCost' :: [Int] -> Int -> Int
fuelCost' crabs dest = sum $ map (costForDistance . (distance dest)) crabs

part2 :: Day7Input -> Result Int
part2 xs = Verified 100727924 $ foldr1 min $ map (fuelCost' xs) $ [lb..ub]
  where
    ub = foldr1 max xs
    lb = foldr1 min xs
