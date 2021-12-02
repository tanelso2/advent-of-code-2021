module Day9
  (
  doDay9
  ) where

import Lib

type Day9Input = ()

parseInput :: String -> Either () Day9Input
parseInput = \x -> Right ()

doDay9 :: IO ()
doDay9 = doDay 9 parseInput part1 part2

part1 :: Day9Input -> Result Int
part1 xs = NotImpl

part2 :: Day9Input -> Result Int
part2 xs = NotImpl
