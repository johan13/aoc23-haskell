module Day01 ( day01p1, day01p2 ) where

import Data.Bifunctor ( Bifunctor(first) )
import Data.Char ( digitToInt, isDigit )
import Data.List ( findIndex, isPrefixOf, minimumBy, tails )
import Data.Maybe ( fromMaybe )
import Data.Ord ( comparing )

day01p1 :: String -> Int
day01p1 = sum . map parseLine1 . lines

day01p2 :: String -> Int
day01p2 = sum . map parseLine2 . lines

parseLine1 :: String -> Int
parseLine1 str =
    let digits = map digitToInt $ filter isDigit str
    in (10 * head digits) + last digits

digitDictionary :: [(String, Int)]
digitDictionary = [
    ("1", 1), ("one", 1),
    ("2", 2), ("two", 2),
    ("3", 3), ("three", 3),
    ("4", 4), ("four", 4),
    ("5", 5), ("five", 5),
    ("6", 6), ("six", 6),
    ("7", 7), ("seven", 7),
    ("8", 8), ("eight", 8),
    ("9", 9), ("nine", 9)]

-- Find the first occurence in 'line' of a dictionary string and return the corresponding int.
findFirst :: [(String, Int)] -> String -> Int
findFirst dictionary line =
    let findPos needle = fromMaybe 1000 $ findIndex (isPrefixOf needle) (tails line)
    in snd $ minimumBy (comparing fst) $ map (first findPos) dictionary

parseLine2 :: String -> Int
parseLine2 line =
    let firstDigit = findFirst digitDictionary line
        lastDigit = findFirst (map (first reverse) digitDictionary) (reverse line)
    in 10 * firstDigit + lastDigit
