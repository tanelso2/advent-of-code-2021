module Day9
  (
  doDay9
  ) where

import Lib

import Data.List
import Data.Maybe
import qualified Data.Set as S

import Grid

import Text.ParserCombinators.Parsec

type Day9Input = Grid Int

parseLine :: Parser [Int]
parseLine = do
  xs <- many1 digit
  optional newline
  return $ map (read . (:[])) xs

parseDay9 = do
  ls <- many parseLine
  return $ grid ls

parseInput :: String -> Either ParseError Day9Input
parseInput = parse parseDay9 ""

doDay9 :: IO ()
doDay9 = doDay 9 parseInput part1 part2

getInGrid = (!?)

getNeighbors :: Grid Int -> (Int,Int) -> [(Int,Int)]
getNeighbors = neighbors'

getNeighborValues :: Grid Int -> (Int,Int) -> [Int]
getNeighborValues g (x,y) = mapMaybe (getInGrid g) $ getNeighbors g (x,y)

isLowPoint :: Grid Int -> (Int,Int) -> Bool
isLowPoint g (x,y) = all (>v) $ getNeighborValues g (x,y)
  where v = fromMaybe e $ getInGrid g (x,y)
        e = error $ "Failed on " ++ (show (x,y))

lowPointValues :: Grid Int -> [Int]
lowPointValues g = mapMaybe (getInGrid g) lowpoints
  where lowpoints = filter (isLowPoint g) $ allpoints g

part1 :: Day9Input -> Result Int
part1 g = Verified 570 res
  where res = sum $ map (+1) $ lowPointValues g

collectBasin :: Grid Int -> (Int,Int) -> [(Int,Int)]
collectBasin g (x,y) = collect [(x,y)] S.empty
  where
    collect [] acc = S.toList acc
    collect (p:ps) acc
      | S.member p acc = collect ps acc
      | v == 9 = collect ps acc
      | otherwise = collect ps' acc'
      where
          ps' = ps ++ (filter ((>v) . fromJust . (getInGrid g)) ns)
          ns = getNeighbors g p
          v = fromJust $ getInGrid g p
          acc' = S.insert p acc

part2 :: Day9Input -> Result Int
part2 g = Verified 899392 $ product $ take 3 $ reverse $ sort $ map (length . (collectBasin g)) $ filter (isLowPoint g) $ allpoints g
