module Day07 (day07p1, day07p2) where

import Data.Char (isDigit)
import Data.Function (on)
import Data.List (group, sort, sortBy)

day07p1 :: String -> Int
day07p1 = sum . scores . sortBy compareHands . parseInput
  where
    scores = zipWith (\rank (_, bid) -> rank * bid) [1..]

day07p2 :: String -> Int
day07p2 = error "TODO"

compareHands :: Hand -> Hand -> Ordering
compareHands (left, _) (right, _) = case (compare `on` handType) left right of
    EQ -> compareCards left right
    x  -> x
  where
    handType x = case sortBy (flip compare) $ map length $ group $ sort x of
        (5:_)   -> 6 :: Int
        (4:_)   -> 5
        (3:2:_) -> 4
        (3:_)   -> 3
        (2:2:_) -> 2
        (2:_)   -> 1
        _       -> 0
    compareCards (l:ls) (r:rs)
        | l == r = compareCards ls rs
        | isDigit l && isDigit r = compare l r
        | l == 'A' = GT
        | r == 'A' = LT
        | l == 'K' = GT
        | r == 'K' = LT
        | l == 'Q' = GT
        | r == 'Q' = LT
        | l == 'J' = GT
        | r == 'J' = LT
        | l == 'T' = GT
        | r == 'T' = LT
    compareCards _ _ = error "Wat?"

type Hand = ([Char], Int) -- cards, bid

parseInput :: String -> [Hand]
parseInput = map (parseRow . words) . lines
  where
    parseRow [cards, bid] = (cards, read bid)
    parseRow _ = error "Invalid input"
