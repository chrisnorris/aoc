module AOC.Y2020.Timings where

import Criterion.Main

import AOC.Y2020.Day1  qualified as D1
import AOC.Y2020.Day2  qualified as D2
import AOC.Y2020.Day3  qualified as D3
import AOC.Y2020.Day4  qualified as D4
import AOC.Y2020.Day5  qualified as D5
import AOC.Y2020.Day6  qualified as D6
import AOC.Y2020.Day7  qualified as D7
import AOC.Y2020.Day8  qualified as D8
import AOC.Y2020.Day8b qualified as D8b
import AOC.Y2020.Day9  qualified as D9
import AOC.Y2020.Day11 qualified as D11
import AOC.Y2020.Day12 qualified as D12
import AOC.Y2020.Day14 qualified as D14
import AOC.Y2020.Day15 qualified as D15
import AOC.Y2020.Day18 qualified as D18
import AOC.Y2020.Day21 qualified as D21
import AOC.Y2020.Day22 qualified as D22
import AOC.Y2020.Day23 qualified as D23
import AOC.Y2020.Day24 qualified as D24
import AOC.Y2020.Day25 qualified as D25

timings = do
 defaultMain [
  bgroup "aoc-2020"
    [ bench "day 1"    $ nfIO D1.main
    , bench "day 2"    $ nfIO D2.main
    , bench "day 3"    $ nfIO D3.main
    , bench "day 4"    $ nfIO D4.main
    , bench "day 5 4b" $ nfIO D5.main4b
    , bench "day 6"    $ nfIO D6.main
    , bench "day 7"    $ nfIO D7.main
    , bench "day 8"    $ nfIO D8.main
    , bench "day 8b"   $ nfIO D8b.main
    , bench "day 9"    $ nfIO D9.main
    , bench "day 11"   $ nfIO D11.main
    , bench "day 12"   $ nfIO D12.main
    , bench "day 14"   $ nfIO D14.main
    , bench "day 15"   $ nfIO D15.main
    , bench "day 18"   $ nfIO D18.main
    , bench "day 21"   $ nfIO D21.main
    , bench "day 22"   $ nfIO D22.main
    , bench "day 23"   $ nfIO D23.main
    , bench "day 24"   $ nfIO D24.main
    , bench "day 25"   $ nfIO D25.main
    ],
  bgroup "aoc-2021" []
  ]
