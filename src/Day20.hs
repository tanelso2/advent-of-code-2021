module Day20
  (
  doDay20
  ) where

import Lib

type Day20Input = ()

parseInput :: String -> Either () Day20Input
parseInput = \x -> Right ()

doDay20 :: IO ()
doDay20 = doDay 20 parseInput part1 part2

part1 :: Day20Input -> Result Int
part1 xs = NotImpl

part2 :: Day20Input -> Result Int
part2 xs = NotImpl
