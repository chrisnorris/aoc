module AOC.Y2024.Day1 where

import AOC.Y2021.Day24 (integer)
import Data.Map.Strict as MS
import Data.Maybe
import Library


main_pt1 = do
  lists <- parseLists
  let sortedForSideBySide = over both (sort . (`zip` [0..])) lists
  return $ sum $ uncurry (zipWith (\(f,_) (h,_) -> abs (h - f))) sortedForSideBySide

main_pt2 = do
  (leftL, rightL) <- parseLists

  let m = Library.foldl' (\ m k -> MS.insertWith (+) k 1 m ) MS.empty rightL
  return $ sum [ x * fromMaybe 0 (m ^. at x) | x <- leftL]

numbers = do
  x <- integer
  spaces
  y <- integer
  return (x,y)

parseLists = do
  input' <- lines <$> readFileY 2024 "d1.input"
  return $ unzip $ sequence (parse numbers "" <$> input') ^. _Right

