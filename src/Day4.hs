module Day4
  (
  doDay4
  ) where

import Data.List
import Text.ParserCombinators.Parsec

import Lib

data BingoSpace =
  Marked Int
  | UnMarked Int
  deriving (Show)

type BingoBoard = [[BingoSpace]]

type Day4Input = ([Int], [BingoBoard])

ntimes :: Int -> Parser a -> Parser [a]
ntimes 1 p = do
  x <- p
  return $ [x]
ntimes n p = do
  x <- p
  xs <- ntimes (n-1) p
  return $ x:xs

parseInt :: Parser Int
parseInt = do
  optional $ many1 $ char ' '
  x <- many1 digit
  return $ read x

parseBingoSpace :: Parser BingoSpace
parseBingoSpace = do
  x <- parseInt
  return $ UnMarked $ x

parseBingoLine :: Parser [BingoSpace]
parseBingoLine = do
  x <- ntimes 5 parseBingoSpace
  optional newline
  return x

parseBingoBoard :: Parser BingoBoard
parseBingoBoard = do
  x <- ntimes 5 parseBingoLine
  optional $ many newline
  return x

parseCallNumbers :: Parser [Int]
parseCallNumbers = do
  x <- sepBy1 parseInt $ oneOf ","
  return x


parseDay4 :: Parser Day4Input
parseDay4 = do
  nums <- parseCallNumbers
  skipMany newline
  boards <- many1 parseBingoBoard
  return (nums, boards)

parseInput :: String -> Either ParseError Day4Input
parseInput s = parse parseDay4 "day4.input" s

doDay4 :: IO ()
doDay4 = do
  -- parseTest parseInt "123"
  -- parseTest parseInt " 123"
  -- parseTest parseInt "12"
  -- parseTest parseInt " 1"
  -- parseTest parseBingoSpace "1"
  -- parseTest parseBingoSpace "45"
  -- parseTest parseBingoLine "1 2 3 4 5"
  -- parseTest parseBingoBoard "30 46 94 20  2\n\
  --                           \53 67 69 75 65\n\
  --                           \27 24 85 28 60\n\
  --                           \57 58 42 36 78\n\
  --                           \35 98 87 91 93\n"

  doDay 4 parseInput part1 part2

columns' :: [[a]] -> [[a]]
columns' xs = foldr1 f $ map (map (:[])) xs
  where
    f line acc = zipWith (++) line acc

markNumber :: Int -> BingoBoard -> BingoBoard
markNumber n b = map (\l -> map f l) b
  where
    f (UnMarked x) = if x == n then Marked x else UnMarked x
    f x = x

marked :: BingoSpace -> Bool
marked (UnMarked _) = False
marked (Marked _) = True

isWinner :: BingoBoard -> Bool
isWinner b = any id $ map (all marked) $ b ++ columns' b

callNumbers :: [Int] -> [BingoBoard] -> Maybe ([BingoBoard], Int)
callNumbers [] _ = Nothing
callNumbers (n:ns) bs = if any isWinner newBoards
                          then Just $ (filter isWinner newBoards, n)
                          else callNumbers ns newBoards
  where newBoards = map (markNumber n) bs

callNumbersFindLast :: [Int] -> [BingoBoard] -> Maybe (BingoBoard, Int)
callNumbersFindLast [] _ = Nothing
callNumbersFindLast (n:ns) bs = if length losers == 0
                                  then Just (head winners, n)
                                  else callNumbersFindLast ns losers
  where newBoards = map (markNumber n) bs
        (winners, losers) = partition isWinner newBoards

boardScore :: BingoBoard -> Int
boardScore b = sum numbers
  where numbers = concatMap (\r -> map f r) b
        f (UnMarked x) = x
        f (Marked _) = 0

part1 :: Day4Input -> Result Int
part1 (c,b) = Res $
  case callNumbers c b of
    Nothing -> 0
    Just (winners, winningNum) ->
      winningNum * (boardScore $ head winners)

part2 :: Day4Input -> Result Int
part2 (c,b) = Res $
  case callNumbersFindLast c b of
    Nothing -> 0
    Just (lastWinner, lastWinningNum) ->
      lastWinningNum * (boardScore lastWinner)
