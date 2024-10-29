module Day05 (day05p1, day05p2) where
import Data.List (find, foldl')
import Data.List.Split (chunksOf, splitOn)

day05p1 :: String -> Int
day05p1 input = let (seeds, maps) = parseInput input
                    locations = foldl' (\loc m -> map (applyMap m) loc) seeds maps
                in minimum locations

day05p2 :: String -> Int
day05p2 input = let (seeds, maps) = parseInput input
                    seedRanges = map (\x -> (head x, head x + last x - 1)) $ chunksOf 2 seeds
                    mappedRanges = foldl' (\r m -> concatMap (applyMap' m) r) seedRanges maps
                in minimum $ map fst mappedRanges

applyMap :: Map -> Int -> Int
applyMap m loc = case find (\(start, end, _) -> start <= loc && loc <= end) m of
    Just (_, _, delta) -> loc + delta
    Nothing -> loc

applyMap' :: Map -> (Int, Int) -> [(Int, Int)]
applyMap' m (lo, hi) = applyOne $ find overlapping m
  where
    overlapping (start, end, _) = start <= hi && end >= lo
    applyOne Nothing = [(lo, hi)]
    applyOne (Just (start, end, delta))
        | start > lo = applyMap' m (lo, start - 1) ++ applyMap' m (start, hi)
        | end < hi = applyMap' m (lo, end) ++ applyMap' m (end + 1, hi)
        | otherwise = [(lo + delta, hi + delta)]

type Input = ([Int], [Map])
type Map = [(Int, Int, Int)]

parseInput :: String -> Input
parseInput input = case lines input of
    (seeds:"":maps) -> (parseSeeds seeds, parseMaps maps)
    _               -> undefined
  where
    parseSeeds = map read . words . drop 6
    parseMaps = map parseMap . splitOn [""]
    parseMap = map parseMapLine . tail
    parseMapLine l = case map read $ words l of
        [dst, src, len] -> (src, src + len - 1, dst - src)
        _               -> undefined
