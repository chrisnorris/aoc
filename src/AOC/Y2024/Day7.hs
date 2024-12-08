module AOC.Y2024.Day7 where

import AOC.Y2021.Day24 (integer)
import Control.Monad.ListT.Funcs (repeatM)
import qualified Data.List.Class as L
import Library

main_pt1 = do
  inp <- asCalibrations
  return $
    sum [r | (r, i) <- inp, r `elem` run i [(+), (*)]]

main_pt2 = do
  inp <- asCalibrations
  return $
    sum [r | (r, i) <- inp, r `elem` run i [(+), (*), (|~)]]

asCalibrations = do
  input' <- lines <$> parseLists
  return $ sequence (parse parseCalibrations "" <$> input') ^. _Right

parseLists = readFileY 2024 "d7.input"

numbers = concat <$> do many1 integer `sepBy` space

parseCalibrations = do
  i <- integer
  char ':'
  n <- numbers
  return (i, n)

run i@(l : ls) s =
  let m = length i
      opTree = L.take (m - 1) (repeatM s)
   in foldl' (\b (a, op) -> op b a) l <$> (zip ls <$> L.toList opTree)

(|~) a b = let s = floor $ logBase 10 (fromIntegral b) in ((10^ (s+1)) * a) + b 