module Day10
  (
  doDay10
  ) where

import Lib

import Data.Either
import Data.List

type Day10Input = [String]

parseInput :: String -> Either () Day10Input
parseInput = \x -> Right $ lines x

doDay10 :: IO ()
doDay10 = do
  -- putStrLn $ show $ processLine [] "{([(<{}[<>[]}>{[]{[(<()>"
  -- putStrLn $ show $ processLine [] "[[<[([]))<([[{}[[()]]]"
  -- putStrLn $ show $ processLine [] "[{[{({}]{}}([{[{{{}}([]"
  -- putStrLn $ show $ processLine [] "[<(<(<(<{}))><([]([]()"
  -- putStrLn $ show $ processLine [] "<{([{{}}[<[[[<>{}]]]>[]]"
  -- putStrLn $ show $ processLine [] "[({(<(())[]>[[{[]{<()<>>"
  doDay 10 parseInput part1 part2

type Stack = [Char]

-- processLine :: curr:cs -> expecting:stack
processLine :: Stack -> [Char] -> Either Char Stack
processLine stack [] = Right $ stack
processLine stack (curr:cs) =
  case curr of
    '{' -> putOnStack '}'
    '[' -> putOnStack ']'
    '<' -> putOnStack '>'
    '(' -> putOnStack ')'
    '}' -> checkExpected
    ']' -> checkExpected
    '>' -> checkExpected
    ')' -> checkExpected
  where
    putOnStack c = processLine (c:stack) cs
    (expected:stackTail) = stack
    checkExpected = if curr == expected
                    then processLine stackTail cs
                    else Left $ curr

scoreForLine :: Either Char Stack -> Int
scoreForLine (Left ')') = 3
scoreForLine (Left ']') = 57
scoreForLine (Left '}') = 1197
scoreForLine (Left '>') = 25137
scoreForLine _ = 0

stackScore :: Stack -> Int
stackScore s = score s 0
  where
    score [] acc = acc
    score (x:xs) acc = score xs $ (acc * 5) + (val x)
    val ')' = 1
    val ']' = 2
    val '}' = 3
    val '>' = 4

middleScore :: [Int] -> Int
middleScore xs = (sort xs) !! i
  where i = length xs `div` 2

part1 :: Day10Input -> Result Int
part1 xs = Verified 318099 $ sum $ map (scoreForLine . (processLine [])) xs

part2 :: Day10Input -> Result Int
part2 xs = Verified 2389738699 $ middleScore $ map stackScore $ rights $ map (processLine []) xs
