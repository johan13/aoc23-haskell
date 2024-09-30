module Day05 (day05p1, day05p2) where

import Data.List (find, foldl')
import Data.List.Split (splitOn)

day05p1 :: String -> Int
day05p1 input = let (seeds, maps) = parseInput input
                in minimum $ map (\s -> foldl' (flip applyMap) s maps) seeds

day05p2 :: String -> Int
day05p2 = error "TODO"

applyMap :: Map -> Int -> Int
applyMap m s = case find (\(_, src, len) -> src <= s && s <= src + len) m of
    Just (dst, src, _) -> s + dst - src
    Nothing -> s

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
