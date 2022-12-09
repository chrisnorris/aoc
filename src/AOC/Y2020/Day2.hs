module AOC.Y2020.Day2 where

import Library


main :: IO ()
main = void day2

parseLine s =
  let (a, b) = break (== ' ') s
      (c, d) = break (== ':') b
      [z] = drop 1 c
      (e, f) = break (== '-') a
      ste = filter (== z) (drop 2 d)
      str = length ste
      i1 = read e :: Int
      i2 = read (drop 1 f) :: Int
   in (i1, i2, str, (str >= i1) && (str <= i2))

day2 =
  map parseLine . lines <$> readFile20 "d2.input"