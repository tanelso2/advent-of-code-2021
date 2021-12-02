module Day17
  (
  doDay17
  ) where

import Lib

type Day17Input = ()

parseInput :: String -> Either () Day17Input
parseInput = \x -> Right ()

doDay17 :: IO ()
doDay17 = doDay 17 parseInput part1 part2

part1 :: Day17Input -> Result Int
part1 xs = NotImpl

part2 :: Day17Input -> Result Int
part2 xs = NotImpl
