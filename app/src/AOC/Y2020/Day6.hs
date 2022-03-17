module AOC.Y2020.Day6 where

import Library
import qualified Data.Set as Set

main :: IO ()
main = void day6

day6 =
  map (length . Set.fromList . concat . words)
    . lines
    . parseFile
    . lines
    <$> readFile20 "d6a.input"