module AOC.Y2020.Day7 where

import Library

main6b =
  readFile' "d6a.input"
    >>= (return . sum)
      . map (length . getIntersects . words)
      . lines
      . parseFile
      . lines
