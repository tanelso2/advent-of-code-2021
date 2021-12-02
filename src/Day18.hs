module Day18
  (
  doDay18
  ) where

import Lib

type Day18Input = ()

parseInput :: String -> Either () Day18Input
parseInput = \x -> Right ()

doDay18 :: IO ()
doDay18 = doDay 18 parseInput part1 part2

part1 :: Day18Input -> Result Int
part1 xs = NotImpl

part2 :: Day18Input -> Result Int
part2 xs = NotImpl
