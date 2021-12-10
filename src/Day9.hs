module Day9
  (
  doDay9
  ) where

import Lib

import Data.List
import Data.List.Extra ((!?))
import Data.Maybe
import qualified Data.Set as S

import Text.ParserCombinators.Parsec

type Grid = [[Int]]

type Day9Input = Grid

parseLine :: Parser [Int]
parseLine = do
  xs <- many1 digit
  optional newline
  return $ map (read . (:[])) xs

parseInput :: String -> Either ParseError Day9Input
parseInput = parse (many parseLine) ""

doDay9 :: IO ()
doDay9 = doDay 9 parseInput part1 part2

getInGrid :: Grid -> (Int,Int) -> Maybe Int
getInGrid g (x,y) = do
  r <- g !? x
  r !? y

getNeighbors :: Grid -> (Int,Int) -> [(Int,Int)]
getNeighbors g (x,y) = filter f candidates
  where
    candidates = [(x+1,y), (x-1,y), (x,y+1),(x,y-1)]
    w = length g
    h = length $ head g
    f (x,y) = x < w && x >= 0 && y < h && y >= 0

getNeighborValues :: Grid -> (Int,Int) -> [Int]
getNeighborValues g (x,y) = mapMaybe (getInGrid g) $ getNeighbors g (x,y)

isLowPoint :: Grid -> (Int,Int) -> Bool
isLowPoint g (x,y) = all (>v) $ getNeighborValues g (x,y)
  where v = fromMaybe e $ getInGrid g (x,y)
        e = error $ "Failed on " ++ (show (x,y))

lowPointValues :: Grid -> [Int]
lowPointValues g = mapMaybe (getInGrid g) lowpoints
  where lowpoints = filter (isLowPoint g) $ allpoints g

allpoints :: Grid -> [(Int,Int)]
allpoints g = do
  x <- init $ [0..(length g)]
  y <- init $ [0..(length $ head g)]
  return (x,y)

part1 :: Day9Input -> Result Int
part1 g = Verified 570 res
  where res = sum $ map (+1) $ lowPointValues g

collectBasin :: Grid -> (Int,Int) -> [(Int,Int)]
collectBasin g (x,y) = collect [(x,y)] S.empty
  where
    collect [] acc = S.toList acc
    collect (p:ps) acc
      | S.member p acc = collect ps acc
      | v == 9 = collect ps acc
      | otherwise = collect ps' acc'
        where
          ps' = ps ++ (filter ((>v) . fromJust . (getInGrid g)) ns
          ns = getNeighbors g p
          v = fromJust $ getInGrid g p
          acc' = S.insert p acc


part2 :: Day9Input -> Result Int
part2 xs = Res $ sum $ take 3 $ sort $ map (length . collectBasin) $ filter (isLowPoint g) $ allpoints g
