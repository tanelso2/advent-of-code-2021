module Day16
  (
  doDay16
  ) where

import Lib

import Text.ParserCombinators.Parsec
import ParseUtils

type Day16Input = Packet

data Packet = 
   Literal PacketHeaders Int 
   | Operator PacketHeaders [Packet]

type PacketHeaders = (Int,Int)

binNum :: Parser Char
binNum = oneOf ['0','1']

parseLiteralVal :: Parser Int
parseLiteralVal = do
   xs <- many middleGroup
   x <- finalGroup
   return readBinString $ xs++[x]

middleGroup = do
  char '0'
  return $ ntimes 4 binNum

finalGroup = do
  char '1'
  return $ ntimes 4 binNum

headers :: Parser PacketHeaders
headers = do
  ver <- ntimes 3 binNum
  type <- ntimes 3 binNum
  return (readBinString ver, readBinString type)

packet :: Parser Packet
packet = do
  (ver,type) <- headers
  case type of
    4 -> Literal (ver, type) $ parseLiteralVal
    _ -> Operator (ver, type) $ operatorChildren

operatorChildren :: Parser [Packet]
operatorChildren = do
  i <- binNum
  if i == 0
  then do
    numBits <- liftM readBinString $ ntimes 15 binNum
    thoseBits <- ntimes numBits binNum
    case parse (many packet) "" thoseBits of
      Left e -> unexpected "Inner parse failed..."
      Right ps -> return ps
  else do
    numPackets <- liftM readBinString $ ntimes 11 binNum
    ntimes numPackets packet

parseInput :: String -> Either ParseError Day16Input
parseInput s = parse packet "" $ hexToBin s

doDay16 :: IO ()
doDay16 = doDay 16 parseInput part1 part2

part1 :: Day16Input -> Result Int
part1 xs = NotImpl

part2 :: Day16Input -> Result Int
part2 xs = NotImpl
