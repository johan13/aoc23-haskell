module Main where

import Aoc23Solutions
import qualified System.Exit as Exit
import Test.HUnit

tests :: Test
tests = TestList [
    TestCase (assertEqual "Day 01 part 1"    54605 . day01p1 =<< readFile "input/input01.txt"),
    TestCase (assertEqual "Day 01 part 2"    55429 . day01p2 =<< readFile "input/input01.txt"),
    TestCase (assertEqual "Day 02 part 1"     2348 . day02p1 =<< readFile "input/input02.txt"),
    TestCase (assertEqual "Day 02 part 2"    76008 . day02p2 =<< readFile "input/input02.txt"),
    TestCase (assertEqual "Day 03 part 1"   525181 . day03p1 =<< readFile "input/input03.txt"),
    TestCase (assertEqual "Day 03 part 2" 84289137 . day03p2 =<< readFile "input/input03.txt")]

main :: IO ()
main = do
    result <- runTestTT tests
    if errors result > 0 || failures result > 0 then Exit.exitFailure else Exit.exitSuccess
