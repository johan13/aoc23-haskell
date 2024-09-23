module Main where

import Aoc23Solutions
import qualified System.Exit as Exit
import Test.HUnit

tests :: Test
tests = TestList [
    TestCase (readFile "input/input01.txt" >>= assertEqual "Day 01 part 1" 54605 . day01p1),
    TestCase (readFile "input/input01.txt" >>= assertEqual "Day 01 part 2" 55429 . day01p2)]

main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
