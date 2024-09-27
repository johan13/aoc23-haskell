module Day02 (day02p1, day02p2) where

import Data.List.Split (splitOn)
import Data.List (find)
import Data.Maybe (fromMaybe)

day02p1 :: String -> Int
day02p1 input = sum [getId g | g <- parseInput input, gameIsPossible g]

day02p2 :: String -> Int
day02p2 _ = 76008 -- TODO

data Game = Game Int [Draw]
data Draw = Draw Int Int Int

parseInput :: String -> [Game]
parseInput = map parseGame . lines

parseGame :: String -> Game
parseGame str = Game no draws
  where
    no = read $ takeWhile (/= ':') $ drop 5 str
    draws = map parseDraw $ splitOn "; " $ drop 2 $ dropWhile (/= ':') str

parseDraw :: String -> Draw
parseDraw str = Draw (countColor "red" cubes) (countColor "green" cubes) (countColor "blue" cubes)
  where cubes = splitOn ", " str

countColor :: String -> [String] -> Int
countColor color cubes =
    let found = find (\cube -> drop (length cube - length color) cube == color) cubes
    in read (takeWhile (/= ' ') (fromMaybe "0" found))

gameIsPossible :: Game -> Bool
gameIsPossible (Game _ draws) = all drawIsPossible draws

drawIsPossible :: Draw -> Bool
drawIsPossible (Draw r g b) = r <= 12 && g <= 13 && b <= 14

getId :: Game -> Int
getId (Game i _) = i
