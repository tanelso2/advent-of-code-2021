module Day25
  (
  doDay25
  ) where

import Lib

type Day25Input = ()

parseInput :: String -> Either () Day25Input
parseInput = \x -> Right ()

doDay25 :: IO ()
doDay25 = doDay 25 parseInput part1 part2

part1 :: Day25Input -> Result Int
part1 xs = NotImpl

part2 :: Day25Input -> Result Int
part2 xs = NotImpl
