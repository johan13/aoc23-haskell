module Day12 (day12p1, day12p2) where
import Control.Monad.Memo (MonadMemo, memo, startEvalMemo)
import Data.List (intercalate)
import Data.List.Split (splitOn)

day12p1 :: String -> Int
day12p1 = sum . map (startEvalMemo . countArrangements) . parseInput

day12p2 :: String -> Int
day12p2 = sum . map (startEvalMemo . countArrangements . unfold) . parseInput
  where
    unfold (lhs, rhs) = (intercalate "?" $ replicate 5 lhs, concat $ replicate 5 rhs)

parseInput :: String -> [([Char], [Int])]
parseInput = map parseLine . lines
  where
    parseLine line = case words line of
        [lhs, rhs] -> (lhs, map read $ splitOn "," rhs)
        _          -> error "Invalid input"

countArrangements :: (MonadMemo ([Char], [Int]) Int m) => ([Char], [Int]) -> m Int
countArrangements ([], []) = return 1
countArrangements ('.':springs, groups) = memo countArrangements (springs, groups)
countArrangements ('?':springs, groups) = do
    a <- memo countArrangements ('#':springs, groups)
    b <- memo countArrangements (springs, groups)
    return (a + b)
countArrangements (springs, group:groups)
    | validGroup = memo countArrangements (drop (group + 1) springs, groups)
      where
        validGroup = '.' `notElem` take group springs &&
            (length springs == group || length springs > group && springs !! group /= '#')
countArrangements _ = return 0
