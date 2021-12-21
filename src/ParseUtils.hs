module ParseUtils
  (
  parseInt
  , int
  , ntimes
  ) where

import Text.ParserCombinators.Parsec
import Data.Maybe

parseInt :: Parser Int
parseInt = do
  optional $ many1 $ char ' '
  neg <- optionMaybe $ char '-'
  x <- many1 digit
  let v = read x
  return $ if isJust neg then -v else v

int = parseInt

ntimes :: Int -> Parser a -> Parser [a]
ntimes 1 p = do
  x <- p
  return $ [x]
ntimes n p = do
  x <- p
  xs <- ntimes (n-1) p
  return $ x:xs
