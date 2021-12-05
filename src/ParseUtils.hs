module ParseUtils
  (
  parseInt
  , int
  , ntimes
  ) where

import Text.ParserCombinators.Parsec

parseInt :: Parser Int
parseInt = do
  optional $ many1 $ char ' '
  x <- many1 digit
  return $ read x

int = parseInt

ntimes :: Int -> Parser a -> Parser [a]
ntimes 1 p = do
  x <- p
  return $ [x]
ntimes n p = do
  x <- p
  xs <- ntimes (n-1) p
  return $ x:xs
