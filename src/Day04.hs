module Day04 (day04p1, day04p2) where
import Data.List (intersect)
import Data.List.Split (splitOn)

day04p1 :: String -> Int
day04p1 = sum . map (score . countWinners) . parseInput
  where
    score 0 = 0
    score n = 2 ^ (n - 1)

day04p2 :: String -> Int
day04p2 = countCards (repeat 1) . map countWinners . parseInput
  where
    countCards (mult:mults) (score:scores) = mult + countCards nextMultipliers scores
      where
        nextMultipliers = zipWith (+) mults (replicate score mult ++ repeat 0)
    countCards _ _ = 0

countWinners :: Card -> Int
countWinners = length . uncurry intersect

type Card = ([Int], [Int])

parseInput :: String -> [Card]
parseInput = map parseLine . lines
  where
    parseLine line = case splitOn "|" $ tail $ dropWhile (/= ':') line of
        [winning, mine] -> (parseNumbers winning, parseNumbers mine)
        _               -> error "Invalid input"
    parseNumbers = map read . words
