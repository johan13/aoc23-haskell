module Day10 (day10p1, day10p2) where
import Data.Set (Set, empty, insert, member, size)

day10p1 :: String -> Int
day10p1 input =
    let pipe = tracePipe (lines input) (53, 75) 'R' -- 'S' is at 53,75 and it covers a '-'.
    in size pipe `div` 2

day10p2 :: String -> Int
day10p2 input =
    let grid = lines input
    in sum $ map countInside $ cleanUpGrid grid $ tracePipe grid (53, 75) 'R'

tracePipe :: [[Char]] -> (Int, Int) -> Char -> Set (Int, Int)
tracePipe grid pos@(x, y) heading = insert pos $
    let x' = x + case heading of 'R' -> 1; 'L' -> -1; _ -> 0
        y' = y + case heading of 'D' -> 1; 'U' -> -1; _ -> 0
    in case (heading, grid !! y' !! x') of
        (_  , 'S') -> empty
        ('R', 'J') -> tracePipe grid (x', y') 'U'
        ('R', '7') -> tracePipe grid (x', y') 'D'
        ('L', 'L') -> tracePipe grid (x', y') 'U'
        ('L', 'F') -> tracePipe grid (x', y') 'D'
        ('U', '7') -> tracePipe grid (x', y') 'L'
        ('U', 'F') -> tracePipe grid (x', y') 'R'
        ('D', 'J') -> tracePipe grid (x', y') 'L'
        ('D', 'L') -> tracePipe grid (x', y') 'R'
        _          -> tracePipe grid (x', y') heading

cleanUpGrid :: [[Char]] -> Set (Int, Int) -> [[Char]]
cleanUpGrid grid pipe = mapWithPos (\pos ch -> if pos `member` pipe then ch else '.') grid
  where
    mapWithPos fn = zipWith (\y -> zipWith (\x c -> fn (x, y) c) [0..]) [0..]

data ScanState = Outside | Inside | BorderInsideUp | BorderInsideDown deriving (Eq)

countInside :: [Char] -> Int
countInside = fst . foldl foldOp (0, Outside)
  where
    foldOp (n, state) ch = (if state == Inside && ch == '.' then n + 1 else n, nextState ch state)
    nextState '|' Outside          = Inside
    nextState '|' Inside           = Outside
    nextState 'L' Outside          = BorderInsideUp
    nextState 'L' Inside           = BorderInsideDown
    nextState 'F' Outside          = BorderInsideDown
    nextState 'F' Inside           = BorderInsideUp
    nextState 'J' BorderInsideUp   = Outside
    nextState 'J' BorderInsideDown = Inside
    nextState '7' BorderInsideUp   = Inside
    nextState '7' BorderInsideDown = Outside
    nextState _ p = p
