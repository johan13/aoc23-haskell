module Day08 (day08p1, day08p2) where
import Data.Map.Strict ((!), fromList, keys, Map)
import Data.List (foldl1', isSuffixOf)

day08p1 :: String -> Int
day08p1 input =
    let (directions, network) = parseInput input
    in findDistance (== "ZZZ") network directions "AAA"

day08p2 :: String -> Int
day08p2 = foldl1' lcm . findPeriods . parseInput
  where
    findPeriods (directions, network) =
        let starts = filter ("A" `isSuffixOf`) (keys network)
        in map (findDistance ("Z" `isSuffixOf`) network directions) starts

findDistance :: (String -> Bool) -> Network -> [Char] -> String -> Int
findDistance _ _ [] _ = undefined
findDistance isEnd network (d:ds) pos
    | isEnd pos = 0
    | otherwise =
        let next = (if d == 'L' then fst else snd) $ network ! pos
        in 1 + findDistance isEnd network ds next

type Network = Map String (String, String)

parseInput :: String -> ([Char], Network)
parseInput input = case lines input of
    (directions:"":nodes) -> (cycle directions, fromList $ map parseNode nodes)
    _                     -> undefined
  where
    parseNode s = (take 3 s, (take 3 $ drop 7 s, take 3 $ drop 12 s))
