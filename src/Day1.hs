module Day1
  (
  doDay1
  ) where

import Lib

type Day1Input = [Int]

parseInput :: String -> Either () Day1Input
parseInput = Right . (map read) . lines

doDay1 :: IO ()
doDay1 = doDay 1 parseInput part1 part2

part1 :: Day1Input -> Result Int
part1 xs = Verified 1387 $ length $ filter z $ zip xs $ tail xs
    where z (a,b) = a < b

part2 :: Day1Input -> Result Int
part2 xs = Verified 1362 $ length $ filter z $ zip windows $ tail windows
  where windows = map (\(a,b,c) -> a+b+c) (zip3 xs (drop 1 xs) (drop 2 xs))
        z (a,b) = a < b
