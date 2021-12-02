module Day21
  (
  doDay21
  ) where

import Lib

type Day21Input = ()

parseInput :: String -> Either () Day21Input
parseInput = \x -> Right ()

doDay21 :: IO ()
doDay21 = doDay 21 parseInput part1 part2

part1 :: Day21Input -> Result Int
part1 xs = NotImpl

part2 :: Day21Input -> Result Int
part2 xs = NotImpl
