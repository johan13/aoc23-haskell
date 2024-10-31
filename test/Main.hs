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
    TestCase (assertEqual "Day 10 part 2"            353 . day10p2 =<< readFile "input/input10.txt"),
    TestCase (assertEqual "Day 11 part 1"        9565386 . day11p1 =<< readFile "input/input11.txt"),
    TestCase (assertEqual "Day 11 part 2"   857986849428 . day11p2 =<< readFile "input/input11.txt"),
    TestCase (assertEqual "Day 12 part 1"           7792 . day12p1 =<< readFile "input/input12.txt"),
    TestCase (assertEqual "Day 12 part 2" 13012052341533 . day12p2 =<< readFile "input/input12.txt"),
    TestCase (assertEqual "Day 13 part 1"          29213 . day13p1 =<< readFile "input/input13.txt"),
    TestCase (assertEqual "Day 13 part 2"          37453 . day13p2 =<< readFile "input/input13.txt"),
    TestCase (assertEqual "Day 14 part 1"         110821 . day14p1 =<< readFile "input/input14.txt"),
    TestCase (assertEqual "Day 14 part 2"          83516 . day14p2 =<< readFile "input/input14.txt")]

main :: IO ()
main = do
    result <- runTestTT tests
    if errors result > 0 || failures result > 0 then Exit.exitFailure else Exit.exitSuccess
