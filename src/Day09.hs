module Day09 (day09p1, day09p2) where

day09p1 :: String -> Int
day09p1 = sum . map (nextInSequence . map read . words) . lines

day09p2 :: String -> Int
day09p2 = sum . map (nextInSequence . reverse . map read . words) . lines

nextInSequence :: [Int] -> Int
nextInSequence x
    | all (== 0) x = 0
    | otherwise = let differences = zipWith (-) (tail x) x
                  in last x + nextInSequence differences
