module Day12
  (
  doDay12
  ) where

import Lib

type Day12Input = ()

parseInput :: String -> Either () Day12Input
parseInput = \x -> Right ()

doDay12 :: IO ()
doDay12 = doDay 12 parseInput part1 part2

part1 :: Day12Input -> Result Int
part1 xs = NotImpl

part2 :: Day12Input -> Result Int
part2 xs = NotImpl
