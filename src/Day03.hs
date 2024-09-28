{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Day03 (day03p1, day03p2) where

import Data.Char (isDigit)

day03p1 :: String -> Int
day03p1 input =
    let items = parseInput input
    in sum [partNo p | p <- items, s <- items, p `isAdjacentTo` s]

day03p2 :: String -> Int
day03p2 input = sum $ map (gearRatio . partsAdjacentToSymbol) $ filter isAsterisk items
  where
    items = parseInput input
    isAsterisk (Symbol _ _ '*') = True
    isAsterisk _ = False
    partsAdjacentToSymbol sym = filter (`isAdjacentTo` sym) items
    gearRatio [p1, p2] = partNo p1 * partNo p2
    gearRatio _ = 0

isAdjacentTo :: Item -> Item -> Bool
isAdjacentTo (PartNo px1 px2 py _) (Symbol sx sy _) =
    py - 1 <= sy && sy <= py + 1 && px1 - 1 <= sx && sx <= px2 + 1
isAdjacentTo _ _ = False

data Item = PartNo { left :: Int, right :: Int, yPos :: Int, partNo :: Int } |
            Symbol { xPos :: Int, yPos :: Int, symbol :: Char }

parseInput :: String -> [Item]
parseInput = concat . zipWith (parseLine 0) [0..] . lines
  where
    parseLine _ _ "" = []
    parseLine x y line@(ch:rest)
        | ch == '.' = parseLine (x + 1) y rest
        | isDigit ch =
            let n = length (takeWhile isDigit line)
            in PartNo x (x + n - 1) y (read $ take n line) : parseLine (x + n) y (drop n line)
        | otherwise = Symbol x y ch : parseLine (x + 1) y rest
