module Day22
  (
  doDay22
  ) where

import Lib

type Day22Input = ()

parseInput :: String -> Either () Day22Input
parseInput = \x -> Right ()

doDay22 :: IO ()
doDay22 = doDay 22 parseInput part1 part2

part1 :: Day22Input -> Result Int
part1 xs = NotImpl

part2 :: Day22Input -> Result Int
part2 xs = NotImpl
