module Day15
  (
  doDay15
  ) where

import Lib

type Day15Input = ()

parseInput :: String -> Either () Day15Input
parseInput = \x -> Right ()

doDay15 :: IO ()
doDay15 = doDay 15 parseInput part1 part2

part1 :: Day15Input -> Result Int
part1 xs = NotImpl

part2 :: Day15Input -> Result Int
part2 xs = NotImpl
