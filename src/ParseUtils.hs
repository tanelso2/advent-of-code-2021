module ParseUtils
  (
  parseInt
  , int
  ) where

import Text.ParserCombinators.Parsec

parseInt :: Parser Int
parseInt = do
  optional $ many1 $ char ' '
  x <- many1 digit
  return $ read x

int = parseInt
