module Day3
  (
  doDay3
  ) where

import Lib

import Data.List

type Day3Input = String

parseInput :: String -> Either () Day3Input
parseInput = Right

doDay3 :: IO ()
doDay3 = doDayWTest 3 parseInput part1 part2 Nothing

columns' :: [[a]] -> [[a]]
columns' xs = foldr1 f $ map (map (:[])) xs
  where
    f line acc = zipWith (++) line acc

columns :: String -> [String]
columns s = foldr1 f $ map (map (:[])) $ lines s
  where
    f line acc = zipWith (++) line acc

bitFlip :: [Char] -> [Char]
bitFlip [] = []
bitFlip ('0':xs) = '1':(bitFlip xs)
bitFlip ('1':xs) = '0':(bitFlip xs)
bitFlip _ = error "Bitflip failed"

readBinary :: String -> Int
readBinary s = snd $ foldr f (1,0) s
  where
    f '0' (place,acc) = (place*2,acc)
    f '1' (place,acc) = (place*2,acc+place)
    f _ _ = error "readBinary was sent something that wasn't 1 or 0"

mostCommon :: [Char] -> Char
mostCommon xs = if o >= z then '1' else '0'
  where
    o = length $ filter (=='1') xs
    z = length $ filter (=='0') xs

mostCommonBin :: [String] -> String
mostCommonBin = (map mostCommon) . columns'

leastCommonBin = bitFlip . mostCommonBin

part1 :: Day3Input -> Result Int
part1 s = Res $ epsilon * gamma
  where
    gamma = readBinary binGamma
    epsilon = readBinary binEpsilon
    binEpsilon = bitFlip binGamma
    binGamma = map mostCommon $ columns s

getCandidate :: String -> [String] -> String
getCandidate x xs = c
  where
    c = f $ sort $ x:xs
    f (b:c:xs) = if b == x then c else f (c:xs)

part2 :: Day3Input -> Result Int
part2 s = Res $ rO2 * rCO2
  where
    bins = lines s
    binO2 = filterDownBy mostCommonBin bins 0
    binCO2 = filterDownBy leastCommonBin bins 0
    rO2 = readBinary binO2
    rCO2 = readBinary binCO2

filterDownBy :: ([String] -> String) -> [String] -> Int -> String
filterDownBy _ [a] _ = a
filterDownBy f xs n =
  filterDownBy f results (n+1)
  where
    x = f xs
    results = filter (\z -> (z !! n) == (x !! n)) xs

exampleInput = [
  "00100"
  , "11110"
  , "10110"
  , "10111"
  , "10101"
  , "01111"
  , "00111"
  , "11100"
  , "10000"
  , "11001"
  , "00010"
  , "01010"
  ]

day3testing :: Day3Input -> IO ()
day3testing s = do
  bins <- return $ lines s
  binO2 <- return $ filterDownBy mostCommonBin bins 0
  putStrLn $ "binO2: " ++ show binO2
  putStrLn $ "Example input: " ++ show (filterDownBy mostCommonBin exampleInput 0)
