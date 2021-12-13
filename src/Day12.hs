module Day12
  (
  doDay12
  ) where

import Lib

import Data.List
import Data.Maybe

import Text.ParserCombinators.Parsec

type Graph a = [(a,a)]
-- just use a list because small dataset
type Set a = [a]

data Cave =
  Start
  | End
  | BigCave String
  | SmallCave String
  deriving (Eq,Show)

readCave :: String -> Cave
readCave "start" = Start
readCave "end" = End
readCave "" = error "No Cave for empty string"
readCave s
  | head s `elem` ['a'..'z'] = SmallCave s
  | otherwise = BigCave s

parseLine :: Parser (Cave,Cave)
parseLine = do
  x <- many1 letter
  char '-'
  y <- many1 letter
  optional newline
  return $ (readCave x, readCave y)

type Day12Input = Graph Cave

parseInput :: String -> Either ParseError Day12Input
parseInput = parse (many parseLine) ""

getPathsFrom :: Graph Cave -> Set Cave -> Cave -> [[Cave]]
getPathsFrom _ _ End = [[End]]
getPathsFrom g visited x = do
  y <- filter (canGoTo visited) $ getNeighbors g x
  p <- filter pathEnds $ getPathsFrom g (x:visited) y
  return $ x:p

canGoTo :: Set Cave -> Cave -> Bool
canGoTo _ Start = False
canGoTo _ End = True
canGoTo _ (BigCave _) = True
canGoTo s x@(SmallCave _) = not $ x `elem` s

pathEnds :: [Cave] -> Bool
pathEnds [] = False
pathEnds [End] = True
pathEnds (_:xs) = pathEnds xs

getNeighbors :: (Eq a) => Graph a -> a -> [a]
getNeighbors g x = mapMaybe f g
  where
    f (i,j)
      | i == x = Just j
      | j == x = Just i
      | otherwise = Nothing

getPaths :: Graph Cave -> [[Cave]]
getPaths g = getPathsFrom g [] Start

doDay12 :: IO ()
doDay12 = doDay 12 parseInput part1 part2

part1 :: Day12Input -> Result Int
part1 xs = Verified 3421 $ length $ getPaths xs

isSmallCave :: Cave -> Bool
isSmallCave (SmallCave _) = True
isSmallCave _ = False

getPathsFrom' :: Graph Cave -> Set Cave -> Bool -> Cave -> [[Cave]]
getPathsFrom' _ _ _ End = [[End]]
getPathsFrom' g visited extraVisitAvailable x = do
  (y,usesExtraVisit) <- mapMaybe f $ getNeighbors g x
  let e' = extraVisitAvailable && not usesExtraVisit
  p <- filter pathEnds $ getPathsFrom' g (x:visited) e' y
  return $ x:p
  where
    f i
      | canGoTo visited i =
        Just (i,False) -- go here, don't use extra visit
      | extraVisitAvailable && isSmallCave i && i `elem` visited =
        Just (i,True) -- go here, use the extra visit
      | otherwise = Nothing -- delete this entry from the list

getPaths' :: Graph Cave -> [[Cave]]
getPaths' g = getPathsFrom' g [] True Start

part2 :: Day12Input -> Result Int
part2 xs = Verified 84870 $ length $ getPaths' xs
