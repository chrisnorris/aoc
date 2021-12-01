module AOC.Y2021.Timings where

import Criterion.Main

import AOC.Y2021.Day1  qualified as D1

timings = do
 defaultMain [
  bgroup "aoc-2021"
    [ bench "day 1"    $ nfIO D1.main
    ]
  ]
