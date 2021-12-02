module Day24
  (
  doDay24
  ) where

import Lib

type Day24Input = ()

parseInput :: String -> Either () Day24Input
parseInput = \x -> Right ()

doDay24 :: IO ()
doDay24 = doDay 24 parseInput part1 part2

part1 :: Day24Input -> Result Int
part1 xs = NotImpl

part2 :: Day24Input -> Result Int
part2 xs = NotImpl
