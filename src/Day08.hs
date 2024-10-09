module Day08 (day08p1, day08p2) where

import Data.Map.Strict (Map, fromList)

day08p1 :: String -> Int
day08p1 = length . snd . parseInput

day08p2 :: String -> Int
day08p2 = error "TODO"

type Network = Map String (String, String)

parseInput :: String -> ([Char], Network)
parseInput input = case lines input of
    (directions:"":nodes) -> (directions, fromList $ map parseNode nodes)
    _                     -> error "Invalid input"
  where
    parseNode s = (take 3 s, (take 3 $ drop 7 s, take 3 $ drop 12 s))
