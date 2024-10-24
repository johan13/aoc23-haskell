module Day07 (day07p1, day07p2) where
import Data.Function (on)
import Data.List (elemIndex, group, partition, sort, sortBy)
import Data.Maybe (fromMaybe)

day07p1 :: String -> Int
day07p1 = totalWinnings handComparer . parseInput
  where
    handComparer = compareType basicCounter <> compareStrength "23456789TJQKA"

day07p2 :: String -> Int
day07p2 = totalWinnings handComparer . parseInput
  where
    handComparer = compareType jokerCounter <> compareStrength "J23456789TQKA"

type Hand = ([Char], Int) -- cards, bid
type HandComparer = Hand -> Hand -> Ordering
type CardCounter = Hand -> [Int]

totalWinnings :: HandComparer -> [Hand] -> Int
totalWinnings handComparer = sum . zipWith winnings [1..] . sortBy handComparer
  where
    winnings rank (_, bid) = rank * bid

-- Compare hand type (two pair, full house etc) without looking at card strength to break ties.
compareType :: CardCounter -> HandComparer
compareType cardCounter = compare `on` cardType . cardCounter
  where
    cardType (5:_)   = 6 :: Int
    cardType (4:_)   = 5
    cardType (3:2:_) = 4
    cardType (3:_)   = 3
    cardType (2:2:_) = 2
    cardType (2:_)   = 1
    cardType _       = 0

-- E.g. "QQQJA" -> [3,1,1]
basicCounter :: CardCounter
basicCounter = sortBy (flip compare) . map length . group . sort . fst

-- E.g. "QQQJA" -> [4,1]
jokerCounter :: CardCounter
jokerCounter (cards, _) =
    let (jokers, other) = partition (== 'J') cards
    in case basicCounter (other, 0) of
        []   -> [length jokers]
        x:xs -> x + length jokers : xs

-- Tiebreaker: compare hands by only looking at card strength.
compareStrength :: [Char] -> HandComparer
compareStrength strengths = compare `on` map cardStrength . fst
  where
    cardStrength c = fromMaybe (error "Invalid card") (elemIndex c strengths)

parseInput :: String -> [Hand]
parseInput = map (parseRow . words) . lines
  where
    parseRow [cards, bid] = (cards, read bid)
    parseRow _ = error "Invalid input"
