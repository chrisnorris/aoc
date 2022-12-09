module AOC.Y2022.Day1 where

import Library

import Data.List

main_pt1 :: IO Int
main_pt1 = maximum <$> getCalories

main_pt2 = sum . take 3 . reverse . sort <$> getCalories

getCalories = (calories <$> inpStr 2022 "d1.input")
 where
  calories =
    map ((sum . map read) . filter notBlank) . groupBy (const notBlank)
  notBlank = (/= mempty)
