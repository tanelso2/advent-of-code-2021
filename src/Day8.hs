module Day8
  (
  doDay8
  ) where

import Lib

import Data.List
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Text.ParserCombinators.Parsec
import ParseUtils

type ScrambledSignals = [String]
type Code = [S.Set Char]
type Day8Input = [(Code, ScrambledSignals)]

parseWord :: Parser [Char]
parseWord = do
  many $ oneOf ['a'..'g']

parseCode :: Parser Code
parseCode = do
  xs <- sepBy parseWord $ char ' '
  return $ map S.fromList xs

parseLine :: Parser (Code, ScrambledSignals)
parseLine = do
  c <- parseCode
  char '|'
  optional spaces
  s <- sepBy parseWord $ char ' '
  optional newline
  return $ (c,s)

parseInput :: String -> Either ParseError Day8Input
parseInput = parse (many1 parseLine) ""

translate :: M.Map Char Char -> String -> String
translate m s = map (m M.!) s

lettersToDigit :: String -> Int
lettersToDigit s =
  case sort s of
    "abcefg" -> 0
    "cf" -> 1
    "acdeg" -> 2
    "acdfg" -> 3
    "bcdf" -> 4
    "abdfg" -> 5
    "abdefg" -> 6
    "acf" -> 7
    "abcdefg" -> 8
    "abcdfg" -> 9

findMap :: Code -> M.Map Char Char
findMap xs = M.fromList $ zip (map (head . S.toList) [a,b,c,d,e,f,g]) "abcdefg"
  where
    l2 = head $ filter (\x -> S.size x == 2) xs
    l3 = head $ filter (\x -> S.size x == 3) xs
    a = S.difference l3 l2
    l5s = filter (\x -> S.size x == 5) xs
    l4 = head $ filter (\x -> S.size x == 4) xs
    abcdf = S.union l3 l4
    r = map (\x -> S.difference x abcdf) l5s
    g = head $ filter (\x -> S.size x == 1) r
    e = head $ filter (\x -> S.size x == 1) $ map (\x -> S.difference x g) r
    aeg = S.unions [a,e,g]
    r2 = map (\x -> S.difference x aeg) l5s
    cd = head $ filter (\x -> S.size x == 2) r2
    f = head $ filter (\x -> S.size x == 1) $ map (\x -> S.difference x cd) r2
    cdf = S.union cd f
    b = head $ filter (\x -> S.size x == 1) $ map (\x -> S.difference x cdf) r2
    d = S.difference cdf l2
    c = S.difference cd d

doDay8 :: IO ()
doDay8 = do
  doDay 8 parseInput part1 part2

decodeSignals :: (Code, ScrambledSignals) -> [Int]
decodeSignals (c,xs) = map (lettersToDigit . translate m) xs
  where
    m = findMap c

part1 :: Day8Input -> Result Int
part1 xs = Verified 479 $ res
  where
    allSignals = concatMap decodeSignals xs
    res = length $ filter (`elem` [1,4,7,8]) allSignals

part2 :: Day8Input -> Result Int
part2 xs = Verified 1041746 $ sum allNums
  where
    signalGroups = map decodeSignals xs
    allNums = map (read . concatMap show) signalGroups
