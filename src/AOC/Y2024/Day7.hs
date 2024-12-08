module AOC.Y2024.Day7 where

import AOC.Y2021.Day24 (integer)
import Control.Monad.ListT.Funcs (repeatM)
import qualified Data.List.Class as L
import Library

main_pt1 = do
  input' <- lines <$> parseLists
  let asCalibrations = sequence (parse parseCalibrations "" <$> input') ^. _Right
  return $
    sum [r | (r, i) <- asCalibrations, r `elem` run i]

parseLists = readFileY 2024 "d7.input"

numbers = concat <$> do many1 integer `sepBy` space

parseCalibrations = do
  i <- integer
  char ':'
  n <- numbers
  return (i, n)

run i@(l : ls) =
  let m = length i
      opTree = L.take (m - 1) (repeatM [(+), (*)])
   in foldl' (\b (a, op) -> op b a) l <$> (zip ls <$> L.toList opTree)
