module Day13 (day13p1, day13p2) where
import Control.Applicative ((<|>))
import Data.List (find, transpose)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)

day13p1 :: String -> Int
day13p1 = solution 0

day13p2 :: String -> Int
day13p2 = solution 1

solution :: Int -> String -> Int
solution smudges = sum . map mirrorScore . splitOn [""] . lines
  where
    mirrorScore pattern = fromMaybe undefined $
        (100*) <$> horizMirror smudges pattern <|> horizMirror smudges (transpose pattern)

horizMirror :: Int -> [[Char]] -> Maybe Int
horizMirror smudges pattern = find isMirror [1..(length pattern - 1)]
  where
    isMirror n = smudges == sum (zipWith delta (reverse $ take n pattern) (drop n pattern))
    delta a b = length $ filter id $ zipWith (/=) a b
