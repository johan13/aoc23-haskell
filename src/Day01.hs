module Day01 (day01p1, day01p2) where

import Data.Char (digitToInt, isDigit)
import Data.List (isPrefixOf, tails)
import Data.Maybe (mapMaybe)

day01p1 :: String -> Int
day01p1 = sum . map (getCalibrationValue . findDigits1) . lines

day01p2 :: String -> Int
day01p2 = sum . map (getCalibrationValue . findDigits2) . lines

getCalibrationValue :: [Int] -> Int
getCalibrationValue digits = 10 * head digits + last digits

findDigits1 :: String -> [Int]
findDigits1 line = [digitToInt x | x <- line, isDigit x]

findDigits2 :: String -> [Int]
findDigits2 line = mapMaybe getPrefixDigit (tails line)
  where
    getPrefixDigit "" = Nothing
    getPrefixDigit str@(x:_)
        | x == '1' || "one"   `isPrefixOf` str = Just 1
        | x == '2' || "two"   `isPrefixOf` str = Just 2
        | x == '3' || "three" `isPrefixOf` str = Just 3
        | x == '4' || "four"  `isPrefixOf` str = Just 4
        | x == '5' || "five"  `isPrefixOf` str = Just 5
        | x == '6' || "six"   `isPrefixOf` str = Just 6
        | x == '7' || "seven" `isPrefixOf` str = Just 7
        | x == '8' || "eight" `isPrefixOf` str = Just 8
        | x == '9' || "nine"  `isPrefixOf` str = Just 9
        | otherwise = Nothing
