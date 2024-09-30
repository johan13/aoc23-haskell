module Day05 (day05p1, day05p2) where

import Data.List (find, foldl', sortBy)
import Data.List.Split (chunksOf, splitOn)
import Data.Ord (comparing)

day05p1 :: String -> Int
day05p1 input = let (seeds, maps) = parseInput input
                in minimum $ map (\s -> foldl' (flip applyMap) s maps) seeds

day05p2 :: String -> Int
day05p2 input = let (seeds, maps) = parseInput input
                    seedRanges = map (\x -> (head x, head x + last x)) $ chunksOf 2 seeds
                    sortedMaps = map (sortBy (comparing (\(_, src, _) -> src))) maps
                    mappedRanges = foldl' (\r m -> concatMap (applyMapToRange m) r) seedRanges sortedMaps
                in minimum $ map fst mappedRanges

applyMap :: Map -> Int -> Int
applyMap m s = case find (\(_, src, len) -> src <= s && s <= src + len) m of
    Just (dst, src, _) -> s + dst - src
    Nothing -> s

applyMapToRange :: Map -> (Int, Int) -> [(Int, Int)]
applyMapToRange m (lo, hi) =
    foo $ find (\(_, src, len) -> src <= hi && src + len - 1 >= lo) m
  where
    foo Nothing = [(lo, hi)]
    foo (Just (dst, src, len))
        | src > lo = (lo, src - 1) : applyMapToRange m (src, hi)
        | src + len - 1 < hi = (lo + dst - src, src + len - 1 + dst - src) : applyMapToRange m (src + len, hi)
        | otherwise = [(lo + dst - src, hi + dst - src)]

type Input = ([Int], [Map])
type Map = [(Int, Int, Int)]

parseInput :: String -> Input
parseInput input = case lines input of
    (seeds:"":maps) -> (parseSeeds seeds, parseMaps maps)
    _               -> error "Invalid input"
  where
    parseSeeds = map read . words . drop 6
    parseMaps x = map parseMap $ splitOn [""] x
    parseMap = map parseMapLine . tail
    parseMapLine l = case map read $ words l of
        [dst, src, len] -> (dst, src, len)
        _               -> error "Invalid input"
