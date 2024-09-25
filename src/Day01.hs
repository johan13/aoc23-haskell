module Day01 (day01p1, day01p2) where

import Data.List (isPrefixOf, tails)
import Data.Maybe (mapMaybe)

day01p1 :: String -> Int
day01p1 = sum . map (getCalibrationValue False) . lines

day01p2 :: String -> Int
day01p2 = sum . map (getCalibrationValue True) . lines

getCalibrationValue :: Bool -> String -> Int
getCalibrationValue checkText line = 10 * head digits + last digits
    where digits = mapMaybe (getPrefixDigit checkText) (tails line)

getPrefixDigit :: Bool -> String -> Maybe Int
getPrefixDigit _ "" = Nothing
getPrefixDigit checkText str@(x:_)
    | x == '1' || checkText && "one"   `isPrefixOf` str = Just 1
    | x == '2' || checkText && "two"   `isPrefixOf` str = Just 2
    | x == '3' || checkText && "three" `isPrefixOf` str = Just 3
    | x == '4' || checkText && "four"  `isPrefixOf` str = Just 4
    | x == '5' || checkText && "five"  `isPrefixOf` str = Just 5
    | x == '6' || checkText && "six"   `isPrefixOf` str = Just 6
    | x == '7' || checkText && "seven" `isPrefixOf` str = Just 7
    | x == '8' || checkText && "eight" `isPrefixOf` str = Just 8
    | x == '9' || checkText && "nine"  `isPrefixOf` str = Just 9
    | otherwise = Nothing
