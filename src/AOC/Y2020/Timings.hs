module AOC.Y2020.Timings where

import Criterion.Main

import qualified AOC.Y2020.Day1  as D1
import qualified AOC.Y2020.Day2  as D2
import qualified AOC.Y2020.Day3  as D3
import qualified AOC.Y2020.Day4  as D4
import qualified AOC.Y2020.Day5  as D5
import qualified AOC.Y2020.Day6  as D6
import qualified AOC.Y2020.Day7  as D7
import qualified AOC.Y2020.Day8  as D8
import qualified AOC.Y2020.Day8b as D8b
import qualified AOC.Y2020.Day9  as D9
import qualified AOC.Y2020.Day11 as D11
import qualified AOC.Y2020.Day12 as D12
import qualified AOC.Y2020.Day14 as D14
import qualified AOC.Y2020.Day15 as D15
import qualified AOC.Y2020.Day18 as D18
import qualified AOC.Y2020.Day21 as D21
import qualified AOC.Y2020.Day22 as D22
import qualified AOC.Y2020.Day23 as D23
import qualified AOC.Y2020.Day24 as D24
import qualified AOC.Y2020.Day25 as D25

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
