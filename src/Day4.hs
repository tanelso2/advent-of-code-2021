module Day4
  (
  doDay4
  ) where

import Lib

type Day4Input = ()

parseInput :: String -> Either () Day4Input
parseInput = \x -> Right ()

doDay4 :: IO ()
doDay4 = doDay 4 parseInput part1 part2

part1 :: Day4Input -> Result Int
part1 xs = NotImpl

part2 :: Day4Input -> Result Int
part2 xs = NotImpl
