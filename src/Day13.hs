module Day13
  (
  doDay13
  ) where

import Lib

import qualified Grid as G

import Text.ParserCombinators.Parsec
import ParseUtils

import Data.List
import qualified Data.Set as S

type Point = (Int,Int)

data Fold =
  HFold Int
  | VFold Int

type Day13Input = ([Point], [Fold])

parsePoint :: Parser Point
parsePoint = do
  x <- int
  char ','
  y <- int
  optional newline
  return (x,y)

parseFold :: Parser Fold
parseFold = do
  string "fold along "
  dir <- oneOf ['x','y']
  char '='
  v <- int
  optional newline
  let constructor = if dir == 'x' then HFold else VFold
  return $ constructor v

parseDay13 :: Parser Day13Input
parseDay13 = do
  ps <- many1 parsePoint
  newline
  fs <- many1 parseFold
  return $ (ps,fs)

parseInput :: String -> Either ParseError Day13Input
parseInput = parse parseDay13 ""

doDay13 :: IO ()
doDay13 = doDay 13 parseInput part1 part2

handleFold :: S.Set Point -> Fold -> S.Set Point
handleFold ps (HFold foldLoc) = S.map (\(x,y) -> (newLoc x foldLoc, y)) ps
handleFold ps (VFold foldLoc) = S.map (\(x,y) -> (x, newLoc y foldLoc)) ps

newLoc :: Int -> Int -> Int
newLoc myLoc foldLoc
  | myLoc < foldLoc = myLoc
  | otherwise = res
    where distance = myLoc - foldLoc
          res = foldLoc - distance

part1 :: Day13Input -> Result Int
part1 (ps,fs) = Res $ S.size $ handleFold (S.fromList ps) (head fs)

part2 :: Day13Input -> Result String
part2 (ps,fs) = Res gString
  where
    finalPs = foldl handleFold (S.fromList ps) fs
    bigX = S.foldr max 0 $ S.map fst finalPs
    bigY = S.foldr max 0 $ S.map snd finalPs
    -- plus ones so that a point (bigX,bigY) will be on the grid
    g = G.gridOfSize (bigX+1) (bigY+1)
    g' = fmap (const ' ') g
    g'' = G.alterBy (\_ p -> S.member p finalPs) (const '#') g'
    gString = G.pprint g''

