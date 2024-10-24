module Day02 (day02p1, day02p2) where
import Data.List (find, isSuffixOf)
import Data.List.Split (splitOn)

day02p1 :: String -> Int
day02p1 = sum . map gameId . filter gameIsPossible . parseInput
  where
    gameIsPossible = all drawIsPossible . draws
    drawIsPossible (r, g, b) = r <= 12 && g <= 13 && b <= 14

day02p2 :: String -> Int
day02p2 = sum . map power . parseInput
  where
    power = product . minCubesRequired . draws
    minCubesRequired = foldr1 (zipWith max) . map (\(r, g, b) -> [r, g, b])

data Game = Game { gameId :: Int, draws :: [(Int, Int, Int)] }

parseInput :: String -> [Game]
parseInput = map parseGame . lines
  where
    parseGame line = case splitOn ": " line of
        [left, right] -> Game (read $ drop 5 left) (map parseDraw $ splitOn "; " right)
        _             -> error "Invalid input"
    parseDraw draw =
        let cubes = splitOn ", " draw
        in (count "red" cubes, count "green" cubes, count "blue" cubes)
    count color = maybe 0 (read . head . words) . find (color `isSuffixOf`)
