module AOC.Y2023.Day9 where

import AOC.Y2021.Day24 (integer)
import Data.Char
import Data.List
import qualified Data.Map as M
import Library

main_pt1 = do
  reports <- concat <$> getOasisReports
  let diffs l = zipWith subtract (init l) (tail l)
  return $
    sum
      ( foldr (\l acc -> acc + last l) 0
          . takeWhile (any (/= 0))
          . iterate diffs
          <$> reports
      )

main_pt2 = do
  reports <- concat <$> getOasisReports
  let diffs l = zipWith subtract (init l) (tail l)
  return $
    sum
      ( foldr (\l acc -> head l - acc) 0
          . takeWhile (any (/= 0))
          . iterate diffs
          <$> reports
      )

getOasisReports = do
  al <- lines <$> readFileY 2023 "d9.input"
  let (Right tuples) = sequence $ parse (many1 integer `sepBy` space) "" <$> al
  return tuples
