module AOC.Y2024.Day5 where

import AOC.Y2021.Day24 (integer)
import Data.List.Split (splitOn)
import Data.Tuple
import Library

data Progress = Start | FoundFirst deriving (Show)

main_pt1 = do
  input' <- parseLists
  let y : ys = splitOn "\n\n" input'
      checkLists = safeParse ordersParser (lines y)
      pages = safeParse numbers (lines $ head ys)
  return $ sum [p !! (round ((fromIntegral l + 1.0) / 2.0) - 1) | p <- pages, let l = length p, not (fails checkOrder checkLists p)]

main_pt2 = do
  input' <- parseLists
  let y : ys = splitOn "\n\n" input'
      checkLists = safeParse ordersParser (lines y)
      pages = safeParse numbers (lines $ head ys)
  return $ [ (p, fails' checkOrder' checkLists p) | p <- pages, let l = length p, fails checkOrder checkLists p]

parseLists = readFileY 2024 "d5.input.sam"

ordersParser = do
  x <- integer
  char '|'
  y <- integer
  return (x, y)

numbers = concat <$> many1 integer `sepBy` char ','

safeParse p s = sequence (parse p "" <$> s) ^. _Right

fails c checkList inp = or $ (\y -> c Start (swap y) inp) <$> checkList
fails' c checkList inp = ((\y -> c Start (swap y) inp) <$> checkList) ^..folded._Just

checkOrder Start (n, m) [] = False
checkOrder FoundFirst (n, m) [] = False
checkOrder FoundFirst (n, m) (x : xs)
  | m == x = True
  | otherwise = checkOrder FoundFirst (n, m) xs
checkOrder Start (n, m) (x : xs)
  | x == n = checkOrder FoundFirst (n, m) xs
  | otherwise = checkOrder Start (n, m) xs

checkOrder' Start (n, m) [] = Nothing
checkOrder' FoundFirst (n, m) [] = Nothing
checkOrder' FoundFirst t@(n, m) (x : xs)
  | m == x = Just t
  | otherwise = checkOrder' FoundFirst (n, m) xs
checkOrder' Start (n, m) (x : xs)
  | x == n = checkOrder' FoundFirst (n, m) xs
  | otherwise = checkOrder' Start (n, m) xs
