module Day2
  (
  doDay2
  ) where

import Lib

data Direction =
  Forward Int
  | Down Int
  | Up Int
  deriving (Show)

type Day2Input = [Direction]

parseLine :: String -> Direction
parseLine s =
  case words s of
    dir:n:[] ->
      case dir of
        "forward" -> Forward $ read n
        "up" -> Up $ read n
        "down" -> Down $ read n
        _ -> error "Not a direction"
    _ -> error "len(words s) != 2"

parseInput :: String -> Either () Day2Input
parseInput = Right . (map parseLine) . lines

doDay2 :: IO ()
doDay2 = doDay 2 parseInput part1 part2

handleDirection (h,d) dir =
  case dir of
    Forward x -> (h+x,d)
    Down x -> (h, d+x)
    Up x -> (h, d-x)

part1 x = Verified 1868935 $ h * d
  where (h,d) = finalPos x
        finalPos = foldl handleDirection (0,0)

handleDirectionWAim (h,d,a) dir =
  case dir of
    Down x -> (h,d,a+x)
    Up x -> (h,d,a-x)
    Forward x -> (h+x,d+(a*x),a)

part2 x = Verified 1965970888 $ h * d
  where (h,d,_) = finalPos x
        finalPos = foldl handleDirectionWAim (0,0,0)

