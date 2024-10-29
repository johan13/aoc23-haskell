module Day06 (day06p1, day06p2) where

day06p1 :: String -> Int
day06p1 = product . map numWaysToWin . parseInput

day06p2 :: String -> Int
day06p2 = numWaysToWin . fixKerning . parseInput
  where
    fixKerning = (\(t, d) -> (read $ concatMap show t, read $ concatMap show d)) . unzip

numWaysToWin :: Game -> Int
numWaysToWin (time, distance) = length $ filter (> distance) $ map getDistance [0..time]
  where
    getDistance holdTime = holdTime * (time - holdTime)

type Game = (Int, Int) -- time, distance

parseInput :: String -> [Game]
parseInput input = case lines input of
    [times, distances] -> zip (parseRow times) (parseRow distances)
    _                  -> undefined
  where
    parseRow = map read . tail . words
