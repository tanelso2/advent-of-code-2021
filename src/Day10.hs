module Day10
  (
  doDay10
  ) where

import Lib

type Day10Input = ()

parseInput :: String -> Either () Day10Input
parseInput = \x -> Right ()

doDay10 :: IO ()
doDay10 = doDay 10 parseInput part1 part2

part1 :: Day10Input -> Result Int
part1 xs = NotImpl

part2 :: Day10Input -> Result Int
part2 xs = NotImpl
