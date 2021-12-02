module Day13
  (
  doDay13
  ) where

import Lib

type Day13Input = ()

parseInput :: String -> Either () Day13Input
parseInput = \x -> Right ()

doDay13 :: IO ()
doDay13 = doDay 13 parseInput part1 part2

part1 :: Day13Input -> Result Int
part1 xs = NotImpl

part2 :: Day13Input -> Result Int
part2 xs = NotImpl
