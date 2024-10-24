{-# LANGUAGE TupleSections #-}
module Day11 (day11p1, day11p2) where

import Data.List (elemIndices, nub)

day11p1 :: String -> Int
day11p1 = sum . map (uncurry distance) . makePairs . expand . parseInput

day11p2 :: String -> Int
day11p2 = error "TODO"

parseInput :: String -> [(Int, Int)]
parseInput = concat . zipWith (\y -> map (, y) . elemIndices '#') [0..] . lines

expand :: [(Int, Int)] -> [(Int, Int)]
expand galaxies =
    let xs = nub $ map fst galaxies
        ys = nub $ map snd galaxies
        emptyCols = [x | x <- [0..maximum xs], x `notElem` xs]
        emptyRows = [y | y <- [0..maximum ys], y `notElem` ys]
        expand' (x, y) = (x + length (filter (< x) emptyCols), y + length (filter (< y) emptyRows))
    in map expand' galaxies

makePairs :: [a] -> [(a, a)]
makePairs [] = []
makePairs [_] = []
makePairs (x:xs) = map (x,) xs ++ makePairs xs

distance :: Num a => (a, a) -> (a, a) -> a
distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)