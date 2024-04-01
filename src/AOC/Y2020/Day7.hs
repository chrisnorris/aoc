module AOC.Y2020.Day7 where

import Library

main :: IO ()
main = void day7

day7 =
  readFile20 "d6a.input"
    >>= (return . sum)
      . map (length . getIntersects . words)
      . lines
      . parseFile
      . lines
