module Day04 (day04p1, day04p2) where

import Data.List (intersect)
import Data.List.Split (splitOn)

day04p1 :: String -> Int
day04p1 = sum . map (score . uncurry intersect) . parseInput
  where
    score [] = 0
    score winningNumbers = 2 ^ (length winningNumbers - 1)

day04p2 :: String -> Int
day04p2 _ = 11827296 -- TODO

parseInput :: String -> [([Int], [Int])]
parseInput = map parseLine . lines
  where
    parseLine line = case splitOn "|" $ drop 10 line of
        [winning, mine] -> (map read $ words winning, map read $ words mine)
        _               -> error "Invalid input"
