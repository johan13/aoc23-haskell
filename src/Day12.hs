module Day12 (day12p1, day12p2) where
import Data.List.Split (splitOn)

day12p1 :: String -> Int
day12p1 = sum . map countArrangements . parseInput

day12p2 :: String -> Int
day12p2 = error "TODO"

parseInput :: String -> [([Char], [Int])]
parseInput = map parseLine . lines
  where
    parseLine line = case words line of
        [lhs, rhs] -> (lhs, map read $ splitOn "," rhs)
        _          -> error "Invalid input"

countArrangements :: ([Char], [Int]) -> Int
countArrangements ([], []) = 1
countArrangements ('.':springs, groups) = countArrangements (springs, groups)
countArrangements ('?':springs, groups) =
    countArrangements ('#':springs, groups) +
    countArrangements (springs, groups)
countArrangements (springs, group:groups)
    | validGroup = countArrangements (drop (group + 1) springs, groups)
      where
        validGroup = '.' `notElem` take group springs &&
            (length springs == group || length springs > group && springs !! group /= '#')
countArrangements _ = 0
