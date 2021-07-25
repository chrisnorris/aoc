module Day1 where

import Lib
import Modules

main =
  (\num -> nub $ traverse %~ sumOf each $ [(x, y, z) | x <- num, y <- num, z <- num, x + y + z == 2020])
    <$> (map read . lines <$> readFile' "d1.input")
