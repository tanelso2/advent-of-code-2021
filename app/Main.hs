module Main where

import Day1
import Day2
import Day3
import Day4
import Day5
import Day6
import Day7
import Day8
import Day9
import Day10
import Day11
import Day12
import Day13
import Day14
import Day15
import Day16
import Day17
import Day18
import Day19
import Day20
import Day21
import Day22
import Day23
import Day24
import Day25

import System.Environment

doReverse = do
  args <- getArgs
  return $ "--reverse" `elem` args

main = do
  r <- doReverse
  allDays <- return $ if r
    then reverse allDays
    else allDays
  foldr1 (>>) allDays

allDays = [
    doDay1
    , doDay2
    , doDay3
    , doDay4
    , doDay5
    , doDay6
    , doDay7
    , doDay8
    , doDay9
    , doDay10
    , doDay11
    , doDay12
    , doDay13
    , doDay14
    -- My solution for day 15, while correct, is insanely slow
    -- , doDay15
    , doDay16
    , doDay17
    , doDay18
    , doDay19
    , doDay20
    , doDay21
    , doDay22
    , doDay23
    , doDay24
    , doDay25
    ]
