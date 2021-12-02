module Day11
  (
  doDay11
  ) where

import Lib

type Day11Input = ()

parseInput :: String -> Either () Day11Input
parseInput = \x -> Right ()

doDay11 :: IO ()
doDay11 = doDay 11 parseInput part1 part2

part1 :: Day11Input -> Result Int
part1 xs = NotImpl

part2 :: Day11Input -> Result Int
part2 xs = NotImpl
