module AOC.Y2020.Day6 where

import Library
import qualified Data.Set as Set

main6a =
  map (length . Set.fromList . concat . words)
    . lines
    . parseFile
    . lines
    <$> readFile' "d6a.input"