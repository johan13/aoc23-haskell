module Day09 (day09p1, day09p2) where

day09p1 :: String -> Int
day09p1 = sum . map (nextInSequence . map read . words) . lines
  where
    nextInSequence x
        | all (== 0) x = 0
        | otherwise = last x + nextInSequence (zipWith (-) (tail x) x)

day09p2 :: String -> Int
day09p2 = sum . map (prevInSequence . map read . words) . lines
  where
    prevInSequence x
        | all (== 0) x = 0
        | otherwise = head x - prevInSequence (zipWith (-) (tail x) x)
