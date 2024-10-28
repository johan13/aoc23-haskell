module Day11 (day11p1, day11p2) where
import Data.List (elemIndices)

day11p1 :: String -> Int
day11p1 = sum . map (uncurry distance) . pairs . expand 2 . parseInput

day11p2 :: String -> Int
day11p2 = sum . map (uncurry distance) . pairs . expand 1000000 . parseInput

type Galaxy = (Int, Int)

parseInput :: String -> [Galaxy]
parseInput = concat . zipWith (\y -> map (, y) . elemIndices '#') [0..] . lines

expand :: Int -> [Galaxy] -> [Galaxy]
expand factor galaxies = map expand' galaxies
  where
    emptyX = [x | x <- [0..], x `notElem` map fst galaxies]
    emptyY = [y | y <- [0..], y `notElem` map snd galaxies]
    expand' (x, y) = (
        x + (factor - 1) * length (takeWhile (< x) emptyX),
        y + (factor - 1) * length (takeWhile (< y) emptyY))

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x:xs) = map (x,) xs ++ pairs xs

distance :: Galaxy -> Galaxy -> Int
distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)
