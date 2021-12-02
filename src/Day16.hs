module Day16
  (
  doDay16
  ) where

import Lib

type Day16Input = ()

parseInput :: String -> Either () Day16Input
parseInput = \x -> Right ()

doDay16 :: IO ()
doDay16 = doDay 16 parseInput part1 part2

part1 :: Day16Input -> Result Int
part1 xs = NotImpl

part2 :: Day16Input -> Result Int
part2 xs = NotImpl
