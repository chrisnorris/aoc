module AOC.Y2022.Day7 where

import           Library

import           Data.List
import           Text.Parsec
import           Text.Parsec.Prim
import           Text.Parsec.Char
import           Text.Parsec.Combinator

-- 1140
main_pt1 = solveLength 4

-- 3495
main_pt2 = solveLength 14

solveLength n = do
  [x] <- inpStr 2022 "d6.input"
  case maximumOf (folded._2) (takeWhile fst $ windows x n) of
    Just res -> return $ res + (n + 1)
    Nothing -> error "rogue input"

windows xs n = (unique <$>  (windowed n xs)) `zip` [0..]
  where unique xs = length (nub xs) /= length xs

windowed n l@(x:xs) = (take n l : windowed n xs)
