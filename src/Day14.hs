module Day14
  (
  doDay14
  ) where

import Lib

type Day14Input = ()

parseInput :: String -> Either () Day14Input
parseInput = \x -> Right ()

doDay14 :: IO ()
doDay14 = doDay 14 parseInput part1 part2

part1 :: Day14Input -> Result Int
part1 xs = NotImpl

part2 :: Day14Input -> Result Int
part2 xs = NotImpl
