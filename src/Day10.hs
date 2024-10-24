module Day10 (day10p1, day10p2) where

day10p1 :: String -> Int
day10p1 input =
    let pipe = tracePipe (lines input) (53, 75) 'R' -- 'S' is at 53,75 and it covers a '-'.
    in length pipe `div` 2

day10p2 :: String -> Int
day10p2 input =
    let grid = lines input
        pipe = tracePipe grid (53, 75) 'R'
        grid' = cleanGrid pipe grid
        posTypes = map (scanl scanner Outside) grid'
    in sum $ map (length . filter (== Inside)) posTypes

tracePipe :: [[Char]] -> (Int, Int) -> Char -> [(Int, Int)]
tracePipe grid pos@(x,y) heading = pos :
    let x' = x + case heading of 'R' -> 1; 'L' -> -1; _ -> 0
        y' = y + case heading of 'D' -> 1; 'U' -> -1; _ -> 0
    in case (heading, grid !! y' !! x') of
        (_  , 'S') -> []
        ('R', 'J') -> tracePipe grid (x', y') 'U'
        ('R', '7') -> tracePipe grid (x', y') 'D'
        ('L', 'L') -> tracePipe grid (x', y') 'U'
        ('L', 'F') -> tracePipe grid (x', y') 'D'
        ('U', '7') -> tracePipe grid (x', y') 'L'
        ('U', 'F') -> tracePipe grid (x', y') 'R'
        ('D', 'J') -> tracePipe grid (x', y') 'L'
        ('D', 'L') -> tracePipe grid (x', y') 'R'
        _          -> tracePipe grid (x', y') heading

-- "pipe -> grid -> cleanGrid" Replace disconnected pipe segments with '.'
cleanGrid :: [(Int, Int)] -> [[Char]] -> [[Char]]
cleanGrid pipe = mapWithPos (\pos c -> if pos `elem` pipe then c else '.')
  where
    mapWithPos fn = zipWith (\y -> zipWith (\x c -> fn (x, y) c) [0..]) [0..]

data PosType = Outside | JustInside | Inside | BorderInsideUp | BorderInsideDown deriving (Eq)

scanner :: PosType -> Char -> PosType
scanner Outside        '|' = JustInside
scanner _              '|' = Outside
scanner Outside        'L' = BorderInsideUp
scanner _              'L' = BorderInsideDown
scanner Outside        'F' = BorderInsideDown
scanner _              'F' = BorderInsideUp
scanner BorderInsideUp 'J' = Outside
scanner _              'J' = JustInside
scanner BorderInsideUp '7' = JustInside
scanner _              '7' = Outside
scanner JustInside     '.' = Inside
scanner p _ = p
