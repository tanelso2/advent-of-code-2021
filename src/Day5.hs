module Day5
  (
  doDay5
  ) where

import Lib
import ParseUtils

import Data.List
import Text.ParserCombinators.Parsec hiding (Line)

type Point = (Int,Int)
type Line = (Point, Point)

isHorizontal :: Line -> Bool
isHorizontal ((_, y1), (_, y2)) = y1 == y2

isVertical :: Line -> Bool
isVertical ((x1, _), (x2, _)) = x1 == x2

isDiagonal :: Line -> Bool
isDiagonal ((x1, y1), (x2, y2)) = x1 /= x2 && y1 /= y2

getHorizontalCoverage :: Line -> [Point]
getHorizontalCoverage ((x1, y1), (x2, y2))
  | x1 >= x2 = f x2 x1
  | x2 > x1 = f x1 x2
  where
    y = y1 -- y1 == y2 in a horizontal line
    f lx hx
      | lx > hx = []
      | otherwise = (lx, y):(f (lx+1) hx)

getVerticalCoverage :: Line -> [Point]
getVerticalCoverage ((x1, y1), (x2, y2))
  | y1 >= y2 = f y2 y1
  | y2 > y1 = f y1 y2
  where
    x = x1 -- x1 == x2 in a vertical line
    f ly hy
      | ly > hy = []
      | otherwise = (x, ly):(f (ly+1) hy)

getDiagonalCoverage :: Line -> [Point]
getDiagonalCoverage ((x1, y1), (x2, y2))
  | x1 == x2 && y1 == y2 = [(x1,y1)]
  | otherwise = (x1,y1):(getDiagonalCoverage (nextPoint, (x2, y2)))
  where
    dx = if x1 > x2 then -1 else 1
    dy = if y1 > y2 then -1 else 1
    nextPoint = ( x1 + dx, y1 + dy )

getCoverage :: Line -> [Point]
getCoverage l
  | isHorizontal l = getHorizontalCoverage l
  | isVertical l = getVerticalCoverage l
  | isDiagonal l = getDiagonalCoverage l
  | otherwise = error "Fuck"

noDiagonals :: [Line] -> [Line]
noDiagonals = filter (\x -> isVertical x || isHorizontal x)

countReoccurences :: [Line] -> Int
countReoccurences ls = length reoccurences
  where allPoints = concatMap getCoverage ls
        groupedPoints = group $ sort allPoints
        reoccurences = filter (\x -> length x > 1) groupedPoints

type Day5Input = [Line]

parsePoint :: Parser Point
parsePoint = do
  x <- int
  char ','
  y <- int
  return $ (x,y)

parseLine :: Parser Line
parseLine = do
  p1 <- parsePoint
  string " -> "
  p2 <- parsePoint
  optional newline
  return (p1,p2)

parseDay5 :: Parser Day5Input
parseDay5 = many parseLine

parseInput :: String -> Either ParseError Day5Input
parseInput = \x -> parse parseDay5 "" x

doDay5 :: IO ()
doDay5 = doDay 5 parseInput part1 part2

part1 :: Day5Input -> Result Int
part1 xs = Verified 5169 $ countReoccurences $ noDiagonals xs

part2 :: Day5Input -> Result Int
part2 xs = Verified 22083 $ countReoccurences $ xs
