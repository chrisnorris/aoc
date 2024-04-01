module AOC.Y2021.Day9 where

import qualified Data.Array as A
import Data.Char (digitToInt)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Library

main = main9i Full

main9i source = do
  (xd, yd) <- liftM2 (,) length (length . head) <$> getInput source

  inputArray <-
    A.listArray ((0, 0), (xd - 1, yd - 1))
      <$> (map digitToInt . concat <$> getInput source)

  let valid (x, y) = x >= 0 && y >= 0 && x < xd && y < yd
      adjacents (x, y) arr =
        [ arr A.! coords
          | m <- [-1, 0, 1],
            n <- [-1, 0, 1],
            let coords = (x + m, y + n),
            valid coords,
            m * m /= n * n
        ]
      lowPoints =
        [ (e, adjacents ix inputArray)
          | (ix, e) <- A.assocs inputArray,
            all (> e) $ adjacents ix inputArray
        ]
      heatMap = sum $ (+ 1) . fst <$> lowPoints
  print heatMap
  where
    getInput source = inp21Str ("d9.input" <> show source)
