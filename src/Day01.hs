module Day01 (day01p1, day01p2) where

import Data.Char (digitToInt, isDigit)
import Data.List (isPrefixOf)

day01p1 :: String -> Int
day01p1 = sum . map getCalibrationValue . lines

day01p2 :: String -> Int
day01p2 = sum . map (getCalibrationValue . replaceTextNumbers) . lines

getCalibrationValue :: String -> Int
getCalibrationValue str = 10 * head digits + last digits
    where digits = [digitToInt x | x <- str, isDigit x]

-- Replace the first letter of a digit written out in text with the corresponding digit. Do not
-- replace the whole word or else we cannot handle overlapping words: "oneightwo" -> "1n8igh2wo"
replaceTextNumbers :: String -> String
replaceTextNumbers [] = []
replaceTextNumbers str@(x:xs) = x' : replaceTextNumbers xs
    where x'
            | "one"   `isPrefixOf` str = '1'
            | "two"   `isPrefixOf` str = '2'
            | "three" `isPrefixOf` str = '3'
            | "four"  `isPrefixOf` str = '4'
            | "five"  `isPrefixOf` str = '5'
            | "six"   `isPrefixOf` str = '6'
            | "seven" `isPrefixOf` str = '7'
            | "eight" `isPrefixOf` str = '8'
            | "nine"  `isPrefixOf` str = '9'
            | otherwise = x
