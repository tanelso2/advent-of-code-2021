module Lib
    (
    Result(..)
    , doDay
    ) where

import Control.Exception

data Result a =
  NotImpl | Res a
  deriving (Show)

maybeDo :: Result a -> (a -> IO ()) -> IO ()
maybeDo NotImpl _ = return ()
maybeDo (Res x) f = f x

printPart dayNum partNum res = do
  putStrLn $ "  Day " ++ show dayNum ++ " part " ++ show partNum ++ " : " ++ show res

filename n = "inputs/" ++ "day" ++ show n ++ ".input"

doDay :: (Show a, Show b, Show e) => Int -> (String -> Either e i) -> (i -> Result a) -> (i -> Result b) -> IO ()
doDay n parseInput part1 part2 = do
  rawOrExc <- try $ readFile $ filename n :: IO (Either IOException String)
  case rawOrExc of
    Left ex -> return () -- putStrLn $ "  Exception: " ++ show ex
    Right raw -> do
      putStrLn $ "~~~ Day " ++ show n ++ " ~~~"
      case parseInput raw of
        Left e -> putStrLn $ "ParseError: " ++ show e
        Right input -> do
          maybeDo (part1 input) $ printPart n 1
          maybeDo (part2 input) $ printPart n 2
      putStrLn ""
