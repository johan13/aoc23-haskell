module Main where

import Aoc23Solutions
import qualified System.Exit as Exit
import Test.HUnit

tests :: Test
tests = TestList [
    TestCase (assertEqual "Day 01 part 1"          54605 . day01p1 =<< readFile "input/input01.txt"),
    TestCase (assertEqual "Day 01 part 2"          55429 . day01p2 =<< readFile "input/input01.txt"),
    TestCase (assertEqual "Day 02 part 1"           2348 . day02p1 =<< readFile "input/input02.txt"),
    TestCase (assertEqual "Day 02 part 2"          76008 . day02p2 =<< readFile "input/input02.txt"),
    TestCase (assertEqual "Day 03 part 1"         525181 . day03p1 =<< readFile "input/input03.txt"),
    TestCase (assertEqual "Day 03 part 2"       84289137 . day03p2 =<< readFile "input/input03.txt"),
    TestCase (assertEqual "Day 04 part 1"          21568 . day04p1 =<< readFile "input/input04.txt"),
    TestCase (assertEqual "Day 04 part 2"       11827296 . day04p2 =<< readFile "input/input04.txt"),
    TestCase (assertEqual "Day 05 part 1"      165788812 . day05p1 =<< readFile "input/input05.txt"),
    TestCase (assertEqual "Day 05 part 2"        1928058 . day05p2 =<< readFile "input/input05.txt"),
    TestCase (assertEqual "Day 06 part 1"         227850 . day06p1 =<< readFile "input/input06.txt"),
    TestCase (assertEqual "Day 06 part 2"       42948149 . day06p2 =<< readFile "input/input06.txt"),
    TestCase (assertEqual "Day 07 part 1"      248569531 . day07p1 =<< readFile "input/input07.txt"),
    TestCase (assertEqual "Day 07 part 2"      250382098 . day07p2 =<< readFile "input/input07.txt"),
    TestCase (assertEqual "Day 08 part 1"          17287 . day08p1 =<< readFile "input/input08.txt"),
    TestCase (assertEqual "Day 08 part 2" 18625484023687 . day08p2 =<< readFile "input/input08.txt"),
    TestCase (assertEqual "Day 09 part 1"     1868368343 . day09p1 =<< readFile "input/input09.txt"),
    TestCase (assertEqual "Day 09 part 2"           1022 . day09p2 =<< readFile "input/input09.txt"),
    TestCase (assertEqual "Day 10 part 1"           6682 . day10p1 =<< readFile "input/input10.txt"),
    TestCase (assertEqual "Day 10 part 2"            353 . day10p2 =<< readFile "input/input10.txt")]

main :: IO ()
main = do
    result <- runTestTT tests
    if errors result > 0 || failures result > 0 then Exit.exitFailure else Exit.exitSuccess
