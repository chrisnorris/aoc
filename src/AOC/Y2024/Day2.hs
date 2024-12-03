module AOC.Y2024.Day2 where

import AOC.Y2021.Day24 (integer)
import Control.Arrow ((&&&))
import Library

main_pt1 = listSize <$> ((safe <$>) <$> parseLists)

main_pt2 =
  parseLists >>= \p ->
     return $ listSize (or . (safe <$>) . dropCycle <$> p)

parseLists = do
  input' <- lines <$> readFileY 2024 "d2.input"
  return $ sequence (parse numbers "" <$> input') ^. _Right

numbers = concat <$> do many1 integer `sepBy` space

dropCycle l = [take x l <> drop (x+1) l | x <- [0..length l - 1]]

safe = uncurry (||) . (all (`elem` [1..3]) &&& all (`elem` [-3 .. -1])) . ap (zipWith (-)) (drop 1)

listSize check = length [ x | x <- check, x]