module AOC.Y2023.Day6 where

import AOC.Y2021.Day24 (integer)
import Data.Maybe
import Library

main_pt1 = do
  t <- readFileY 2023 "d6.input"
  let (Right races) = uncurry zip <$> parse (liftM2 (,) seedsP seedsP) "" t
  return $ foldBy (*) 1 $ length . race <$> races

seedsP = do many1 letter; char ':'; numbers

numbers = concat <$> do many1 integer `sepBy` space

race (time, record) = filter (> record) [x * (time - x) | x <- [0 .. time]]

main_pt2 = do
  (Right races) <- parse (liftM2 (,) seedsP2 seedsP2) "" <$> readFileY 2023 "d6.input"
  return $ length $ race races

seedsP2 = read . concatMap show <$> do many1 letter; char ':'; numbers
