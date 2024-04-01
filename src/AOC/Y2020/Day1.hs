module AOC.Y2020.Day1 where

import Library

main :: IO ()
main = void $ day1

day1 =
  ( \num ->
      nub
        $ traverse
          %~ sumOf each
        $ [(x, y, z) | x <- num, y <- num, z <- num, x + y + z == 2020]
  )
    <$> (map read . lines <$> readFile20 "d1.input")
