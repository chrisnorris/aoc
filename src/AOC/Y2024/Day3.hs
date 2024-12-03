module AOC.Y2024.Day3 where

import AOC.Y2021.Day24 (integer)
-- import Control.Arrow ((&&&))

import Data.Either
import Data.List.Split
import Library

main_pt1 = do
  input' <- parseLists
  let parsed = ((parse (try mulI) "" <$>) . splitOn "mul") input'
  return $ sum $ uncurry (*) <$> (parsed ^.. traverse . _Right)

main_pt2 = do
  input' <- parseLists
  let parsed = (((\x -> (parse (try mulI) "" x, switch x)) <$>) . splitOn "mul") input'
  return $ doOrDontMul parsed

doOrDontMul = go 1 0
  where
    go dodont s [] = s
    go dodont s ((Left _, dd) : xs)
      | dd == -1 = go dodont s xs
      | dd == 0 = go 0 s xs
      | dd == 1 = go 1 s xs
    go dodont s ((Right t, dd) : xs)
      | dd == -1 = go dodont newSum xs
      | dd == 0 = go 0 newSum xs
      | dd == 1 = go 1 newSum xs
      where
        newSum = s + dodont * uncurry (*) t

switch (hasSubString "don't" -> True) = 0
switch (hasSubString "do" -> True) = 1
switch _ = -1

hasSubString s = isRight . parse hasItem ""
  where
    hasItem = prefixItem <* many anyChar
    prefixItem = try item <|> (anyChar >> prefixItem)
    item = string s

parseLists = readFileY 2024 "d3.input"

mulI =
  between
    (char '(')
    (char ')')
    ( do
        x <- integer
        char ','
        y <- integer
        return (x, y)
    )
