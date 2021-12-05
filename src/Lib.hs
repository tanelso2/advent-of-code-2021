module Lib
    (
    Result(..)
    , doDay
    , doDayWTest
    ) where

import Control.Exception

data Result a =
  NotImpl
  | Res a
  | InProgress (IO ())
  | Verified a a

maybeDo :: (Eq a, Show a) => Result a -> (a -> IO ()) -> IO ()
maybeDo NotImpl _ = return ()
maybeDo (Res x) f = f x
maybeDo (InProgress io) _ = io
maybeDo (Verified x y) f
  | x == y = do
    putStr "✓"
    maybeDo (Res x) f
  | otherwise = do
    putStrLn $ "Could not verify: Expected " ++ show x ++ " /= Result " ++ show y

printPart dayNum partNum res = do
  putStrLn $ "  Day " ++ show dayNum ++ " part " ++ show partNum ++ " : " ++ show res

filename n = "inputs/" ++ "day" ++ show n ++ ".input"

doDay n parseInput p1 p2 = doDayWTest n parseInput p1 p2 Nothing

doDayWTest :: (Show a, Show b, Show e, Eq a, Eq b) => Int -> (String -> Either e i) -> (i -> Result a) -> (i -> Result b) -> Maybe (i -> IO ()) -> IO ()
doDayWTest n parseInput part1 part2 maybeTest = do
  rawOrExc <- try $ readFile $ filename n :: IO (Either IOException String)
  case rawOrExc of
    Left ex -> return () -- putStrLn $ "  Exception: " ++ show ex
    Right raw -> do
      putStrLn $ "~~~ Day " ++ show n ++ " ~~~"
      case parseInput raw of
        Left e -> putStrLn $ "ParseError: " ++ show e
        Right input -> do
          case maybeTest of
            Nothing -> return ()
            Just f -> do
              putStrLn " - TESTING - "
              f input
          maybeDo (part1 input) $ printPart n 1
          maybeDo (part2 input) $ printPart n 2
      putStrLn ""
