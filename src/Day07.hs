module Day07 (day07p1, day07p2) where

import Data.Function (on)
import Data.List (elemIndex, group, sort, sortBy)
import Data.Maybe (fromMaybe)

day07p1 :: String -> Int
day07p1 = sum . scores . sortBy compareHands . parseInput
  where
    scores = zipWith (\rank (_, bid) -> rank * bid) [1..]

day07p2 :: String -> Int
day07p2 = sum . scores . sortBy compareHands' . parseInput
  where
    scores = zipWith (\rank (_, bid) -> rank * bid) [1..]

compareHands :: Hand -> Hand -> Ordering
compareHands (left, _) (right, _) = case (compare `on` handValue) left right of
    EQ -> (compare `on` map cardValue) left right
    x  -> x
  where
    handValue x = case sortBy (flip compare) $ map length $ group $ sort x of
        (5:_)   -> 6 :: Int
        (4:_)   -> 5
        (3:2:_) -> 4
        (3:_)   -> 3
        (2:2:_) -> 2
        (2:_)   -> 1
        _       -> 0
    cardValue c = fromMaybe (error "Invalid card") (elemIndex c "23456789TJQKA")

compareHands' :: Hand -> Hand -> Ordering
compareHands' (left, _) (right, _) = case (compare `on` handValue) left right of
    EQ -> (compare `on` map cardValue) left right
    x  -> x
  where
    handValue x = case foo x of
        (5:_)   -> 6 :: Int
        (4:_)   -> 5
        (3:2:_) -> 4
        (3:_)   -> 3
        (2:2:_) -> 2
        (2:_)   -> 1
        _       -> 0
    cardValue c = fromMaybe (error "Invalid card") (elemIndex c "J23456789TQKA")
    foo :: [Char] -> [Int] -- TODO: Handle J as wildcard
    foo x = sortBy (flip compare) $ map length $ group $ sort x

type Hand = ([Char], Int) -- cards, bid

parseInput :: String -> [Hand]
parseInput = map (parseRow . words) . lines
  where
    parseRow [cards, bid] = (cards, read bid)
    parseRow _ = error "Invalid input"
