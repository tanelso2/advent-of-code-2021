module Day17
  (
  doDay17
  ) where

import Lib

import Text.ParserCombinators.Parsec
import ParseUtils

import Data.List

type TargetArea = (Int, Int, Int, Int)

type Probe = (Int, Int, Int, Int)

type Day17Input = TargetArea

targetArea :: Parser TargetArea
targetArea = do
  string "target area: x="
  lx <- int
  string ".."
  hx <- int
  string ", y="
  ly <- int
  string ".."
  hy <- int
  return (lx,hx,ly,hy)

parseInput :: String -> Either ParseError Day17Input
parseInput = parse targetArea ""

doDay17 :: IO ()
doDay17 = doDay 17 parseInput part1 part2

towards :: (Num a, Ord a) => a -> a -> a
towards target x
  | x < target = x + 1
  | x > target = x - 1
  | otherwise = x

iterateProbe :: Probe -> Probe
iterateProbe (x,y,dx,dy) = (x + dx, y + dy, dx', dy')
  where
    dy' = dy - 1
    dx' = towards 0 dx

getTrajectory :: (Int, Int) -> [Probe]
getTrajectory (dx,dy) = iterate iterateProbe (0,0,dx,dy)

tryTrajectories :: TargetArea -> [[Probe]]
tryTrajectories (lx,hx,ly,hy)= do
  let b = 100
  x <- [0..hx]
  y <- [ly..b]
  return $ getTrajectory (x,y)

inTargetArea :: TargetArea -> Probe -> Bool
inTargetArea (lx,hx,ly,hy) (x,y,_,_) = all id $
  [
  lx <= x
  , hx >= x
  , ly <= y
  , hy >= y
  ]

passesThrough :: TargetArea -> [Probe] -> Bool
passesThrough ta ps = any (inTargetArea ta) ps'
  where
    (_,_,ly,_) = ta
    ps' = takeWhile (\(_,y,_,_) -> y >= ly) ps

maxHeight :: [Probe] -> Int
maxHeight ps = r
  where
    ys = map (\(_,y,_,_) -> y) ps
    zs = zip ys (tail ys)
    (r,_) = head $ dropWhile (\(h,h') -> h < h') zs

part1 :: Day17Input -> Result Int
part1 ta = Verified 2850 $ foldr1 max $ map maxHeight $ filter (passesThrough ta) $ tryTrajectories ta

part2 :: Day17Input -> Result Int
part2 ta = Verified 1117 $ length $ filter (passesThrough ta) $ tryTrajectories ta
