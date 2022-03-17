module AOC.Y2020.Day11 where

import Library
import qualified Data.Array as A


main = do
  x <- concat . lines <$> readFile20 "d11.input"
  y <- concat . lines <$> readFile20 "d11.input.sam2"

  let arrInit = A.listArray ((0, 0), (97, 94)) x

      valid (x, y) = x >= 0 && y >= 0 && x < 98 && y < 95
      adjacents (x, y) arr =
        [ arr A.! (x + m, y + n)
          | m <- [-1, 0, 1],
            n <- [-1, 0, 1],
            valid (x + m, y + n),
            (m, n) /= (0, 0)
        ]
      newSeat arr (x, y) =
        let nowSeat = arr A.! (x, y)
            adj = adjacents (x, y) arr
            noOccupieds = '#' `notElem` adj
         in case nowSeat of
           
              -- If a seat is occupied (#) and four or more seats adjacent to it are also occupied, the seat becomes empty.
              '#' -> if length (filter (== '#') adj) >= 4 then 'L' else '#'

              -- Otherwise, the seat's state does not change.
              '.' -> '.'

              -- If a seat is empty (L) and there are no occupied seats adjacent to it, the seat becomes occupied.
              'L' -> if noOccupieds then '#' else 'L'

      loop n arrStart arr =
         if arrStart == arr
            then return (n, arrStart)
            else
              loop
                (n + 1)
                arr
                ( A.listArray
                    ((0, 0), (97, 94))
                    [newSeat arr (i, j) | i <- [0 .. 97], j <- [0 .. 94]]
                )

  loop 0 (A.listArray ((0, 0), (97, 94)) $ repeat '.') arrInit

pretty11 ::
  (Show a1, Num a2, Num b, Enum a2, Enum b, A.Ix a2, A.Ix b) =>
  A.Array (a2, b) a1 ->
  IO ()
pretty11 arr = forM_ [0 .. 9] $ \x ->
  print $
    take 10 $
      drop
        (10 * x)
        [arr A.! (i, j) | i <- [0 .. 9], j <- [0 .. 9]]