module Day16
  (
  doDay16
  ) where

import Lib

import Control.Monad

import Text.ParserCombinators.Parsec
import ParseUtils

type Day16Input = Packet

data Packet = 
   Literal PacketHeaders Int 
   | Operator PacketHeaders [Packet]
   deriving (Show)

type PacketHeaders = (Int,Int)

binNum :: Parser Char
binNum = oneOf ['0','1']

parseLiteralVal :: Parser Int
parseLiteralVal = do
   xs <- many middleGroup
   x <- finalGroup
   return $ readBinString $ concat $ xs ++ [x]

middleGroup :: Parser [Char]
middleGroup = do
  char '0'
  ntimes 4 binNum

finalGroup :: Parser [Char]
finalGroup = do
  char '1'
  ntimes 4 binNum

headers :: Parser PacketHeaders
headers = do
  ver <- ntimes 3 binNum
  ty <- ntimes 3 binNum
  return (readBinString ver, readBinString ty)

packet :: Parser Packet
packet = do
  (ver,ty) <- headers
  case ty of
    4 -> do
      v <- parseLiteralVal
      return $ Literal (ver, ty) v
    _ -> do
      ps <- operatorChildren
      return $ Operator (ver, ty) ps

operatorChildren :: Parser [Packet]
operatorChildren = do
  i <- binNum
  if i == '0'
  then do
    numBits <- liftM readBinString $ ntimes 15 binNum
    thoseBits <- ntimes numBits binNum
    case parse (many packet) "" thoseBits of
      Left e -> unexpected $ "Inner parse failed on: " ++ thoseBits
      Right ps -> return ps
  else do
    numPackets <- liftM readBinString $ ntimes 11 binNum
    ntimes numPackets packet

parseInput :: String -> Either ParseError Day16Input
parseInput s = parse packet "" $ hexToBin s

hexToBin :: String -> String
hexToBin [] = []
hexToBin (x:xs) = v ++ hexToBin xs
  where
    v = case x of
          '0' -> "0000"
          '1' -> "0001"
          '2' -> "0010"
          '3' -> "0011"
          '4' -> "0100"
          '5' -> "0101"
          '6' -> "0110"
          '7' -> "0111"
          '8' -> "1000"
          '9' -> "1001"
          'A' -> "1010"
          'B' -> "1011"
          'C' -> "1100"
          'D' -> "1101"
          'E' -> "1110"
          'F' -> "1111"

readBinString :: String -> Int
readBinString s = snd $ foldr f (1,0) s
  where
    f '0' (place,acc) = (place*2,acc)
    f '1' (place,acc) = (place*2,acc+place)
    f _ _ = error "readBinary was sent something that wasn't 1 or 0"

doDay16 :: IO ()
doDay16 = do
  putStrLn $ show $ parseInput "D2FE28"
  putStrLn $ show $ parseInput "38006F45291200"
  doDay 16 parseInput part1 part2

score :: Packet -> Int
score (Literal (v,_) _) = v
score (Operator (v,_) ps) = v + (sum $ map score ps)

part1 :: Day16Input -> Result Int
part1 xs = Res $ score xs

part2 :: Day16Input -> Result Int
part2 xs = NotImpl
