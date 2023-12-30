module AOC.Y2023.Day1 where

import Control.Arrow ((&&&))
import Data.Char
import Data.Maybe

import Library

main_pt1 :: IO Int
main_pt1 = do
  input' <-  ((\s -> getStartEnd <$> [s, reverse s]) <$>) . lines <$>
    readFileY 2023 "d1.input"
  return $ sum $ read <$> input'

getStartEnd l = head (dropWhile isAlpha l)

main_pt2 :: IO Int
main_pt2 = do
  y <- lines <$> readFileY 2023 "d1.input"
  return $ sum $ read . (^.. both) <$>
    ((loop id [] &&& loop reverse [] . reverse) <$> y)

loop f acc [] = head acc
loop f acc s@(l:ls) = 
  if isDigit l then loop f (acc <> [l]) ls else 
  case switchTodigits s (f <$> digitsAsWords) of
    (Nothing, _) -> loop f acc ls
    (Just p, b) -> loop f (acc<>[intToDigit p]) b

switchTodigits str daw =
  let m = (\w -> str ^? prefixed w) <$> daw in
  case findIndexOf traversed isJust m of
    Just x ->  (Just $ x+1, fromJust $ m ^. traversed.filtered isJust)
    Nothing -> (Nothing, str)

digitsAsWords =  ["one","two","three","four","five","six","seven","eight","nine"]
