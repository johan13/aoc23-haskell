module Main where

import Aoc23Solutions
import qualified System.Exit as Exit
import Test.HUnit

tests :: Test
tests = TestList [
    TestCase (day01p1 >>= assertEqual "Day 01 part 1" 54605),
    TestCase (day01p2 >>= assertEqual "Day 01 part 2" 55429)
    ]

main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
