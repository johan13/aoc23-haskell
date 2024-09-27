module Day02 (day02p1, day02p2) where

import Data.List (find, isSuffixOf)
import Data.List.Split (splitOn)

day02p1 :: String -> Int
day02p1 = sum . map getId . filter gameIsPossible . parseInput
  where
    getId (Game i _) = i
    gameIsPossible (Game _ draws) = all drawIsPossible draws
    drawIsPossible (Draw r g b) = r <= 12 && g <= 13 && b <= 14

day02p2 :: String -> Int
day02p2 = sum . map power . parseInput
  where
    power = product . foldr1 (zipWith max) . getAllDraws
    getAllDraws (Game _ draws) = map drawToList draws
    drawToList (Draw r g b) = [r, g, b]

data Game = Game Int [Draw]
data Draw = Draw Int Int Int

parseInput :: String -> [Game]
parseInput = map parseGame . lines
  where
    parseGame line = case splitOn ": " line of
        [label, draws] -> Game (read $ drop 5 label) (map parseDraw $ splitOn "; " draws)
        _              -> error "Invalid input"
    parseDraw draw =
        let cubes = splitOn ", " draw
        in Draw (count "red" cubes) (count "green" cubes) (count "blue" cubes)
    count color = maybe 0 (read . head . words) . find (color `isSuffixOf`)
