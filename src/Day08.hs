module Day08 (day08p1, day08p2) where

import Data.Map.Strict (Map, fromList, (!))

day08p1 :: String -> Int
day08p1 = follow "AAA" . parseInput
  where
    follow _ ([], _) = 0
    follow "ZZZ" _ = 0
    follow current (i:instr, network) =
        let next = (if i == 'L' then fst else snd) $ network ! current
        in 1 + follow next (instr, network)

day08p2 :: String -> Int
day08p2 = error "TODO"

type Network = Map String (String, String)

parseInput :: String -> ([Char], Network)
parseInput input = case lines input of
    (directions:"":nodes) -> (cycle directions, fromList $ map parseNode nodes)
    _                     -> error "Invalid input"
  where
    parseNode s = (take 3 s, (take 3 $ drop 7 s, take 3 $ drop 12 s))
