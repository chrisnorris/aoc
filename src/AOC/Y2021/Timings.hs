module AOC.Y2021.Timings where

import qualified AOC.Y2021.Day1 as D1
import qualified AOC.Y2021.Day2 as D2
import Criterion.Main

timings = do
  defaultMain
    [ bgroup
        "aoc-2021"
        [ bench "day 1" $ nfIO D1.main,
          bench "day 2" $ nfIO D2.main2,
          bench "day 2ii" $ nfIO D2.main2ii
        ]
    ]
