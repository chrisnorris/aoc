module AOC.Y2021.Timings where

import Criterion.Main

import AOC.Y2021.Day1  qualified as D1
import AOC.Y2021.Day2  qualified as D2

timings = do
 defaultMain [
  bgroup "aoc-2021"
    [ bench "day 1"    $ nfIO D1.main
    , bench "day 2"    $ nfIO D2.main2
    , bench "day 2ii"  $ nfIO D2.main2ii
    ]
  ]
