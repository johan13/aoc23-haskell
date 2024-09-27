{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Day03 (day03p1, day03p2) where

import Data.Char (isDigit)

day03p1 :: String -> Int
day03p1 input =
    let items = parseInput input
    in sum [partNo p | p <- items, s <- items, isAdjacent p s]

day03p2 :: String -> Int
day03p2 input = sum $ map gearRatioOfTwoGears partsAdjacentToGears
  where
    items = parseInput input
    isGear (Symbol _ _ '*') = True
    isGear _ = False
    partsAdjacentToGears = map (\s -> filter (`isAdjacent` s) items) (filter isGear items)
    gearRatioOfTwoGears [p1, p2] = partNo p1 * partNo p2
    gearRatioOfTwoGears _ = 0

isAdjacent :: Input -> Input -> Bool
isAdjacent (PartNo px1 px2 py _) (Symbol sx sy _) =
    py - 1 <= sy && sy <= py + 1 && px1 - 1 <= sx && sx <= px2 + 1
isAdjacent _ _ = False

data Input = PartNo { xLeft :: Int, xRight :: Int, yPos :: Int, partNo :: Int } |
             Symbol { xPos :: Int, yPos :: Int, symbol :: Char }

parseInput :: String -> [Input]
parseInput = concat . zipWith (parseLine 0) [0..] . lines
  where
    parseLine _ _ "" = []
    parseLine x y line@(ch:rest)
        | ch == '.' = parseLine (x + 1) y rest
        | isDigit ch =
            let n = length (takeWhile isDigit line)
            in PartNo x (x + n - 1) y (read $ take n line) : parseLine (x + n) y (drop n line)
        | otherwise = Symbol x y ch : parseLine (x + 1) y rest
