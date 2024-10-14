module Day10 (day10p1, day10p2) where

day10p1 :: String -> Int
day10p1 = farthestPoint . lines
  where
    -- S is at 53,75 and it covers a '-'. Start at 54,75 going right and add 1 to the distance.
    farthestPoint grid = (1 + measureLoop grid (54, 75) 'R') `div` 2
    measureLoop grid (x, y) heading = 1 + case grid !! y !! x of
        'S' -> 0
        'J' | heading == 'R' -> measureLoop grid (x, y - 1) 'U'
        '-' | heading == 'R' -> measureLoop grid (x + 1, y) 'R'
        '7' | heading == 'R' -> measureLoop grid (x, y + 1) 'D'
        'L' | heading == 'L' -> measureLoop grid (x, y - 1) 'U'
        '-' | heading == 'L' -> measureLoop grid (x - 1, y) 'L'
        'F' | heading == 'L' -> measureLoop grid (x, y + 1) 'D'
        '7' | heading == 'U' -> measureLoop grid (x - 1, y) 'L'
        '|' | heading == 'U' -> measureLoop grid (x, y - 1) 'U'
        'F' | heading == 'U' -> measureLoop grid (x + 1, y) 'R'
        'J' | heading == 'D' -> measureLoop grid (x - 1, y) 'L'
        '|' | heading == 'D' -> measureLoop grid (x, y + 1) 'D'
        'L' | heading == 'D' -> measureLoop grid (x + 1, y) 'R'
        _ -> error "Invalid map"

day10p2 :: String -> Int
day10p2 = error "TODO"
