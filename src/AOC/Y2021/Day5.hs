module AOC.Y2021.Day5 where

import Library

main =
  main5 Sample segment -- 5
    >> main5 Sample segmentWithDiagonal -- 12
    >> main5 Full segment -- 6548
    >> main5 Full segmentWithDiagonal -- 19663

main5 source strategy =
  readInstr source
    >>= print
      . (length . filter ((> 1) . length) . group . sort . (uncurry strategy =<<))

segment (x1, y1) (x2, y2)
  | x1 == x2 = [(i, j) | i <- [x1], j <- if y1 > y2 then [y2 .. y1] else [y1 .. y2]]
  | y1 == y2 = [(i, j) | i <- if x1 > x2 then [x2 .. x1] else [x1 .. x2], j <- [y1]]
  | otherwise = []

segmentWithDiagonal (x1, y1) (x2, y2)
  | x1 == x2 =
      [(i, j) | i <- [x1], j <- if y1 > y2 then [y2 .. y1] else [y1 .. y2]]
  | y1 == y2 =
      [(i, j) | i <- if x1 > x2 then [x2 .. x1] else [x1 .. x2], j <- [y1]]
  | isDiagonal (x1, y1) (x2, y2) =
      (if x1 > x2 then reverse [x2 .. x1] else [x1 .. x2])
        `zip` (if y1 > y2 then reverse [y2 .. y1] else [y1 .. y2])

isDiagonal (x1, y1) (x2, y2) =
  abs (fromIntegral (x1 - x2) / fromIntegral (y1 - y2)) == 1.0

readInstr :: Source -> IO [(PairInts, PairInts)]
readInstr source =
  (read . (\[a, b, c] -> "((" <> a <> "),(" <> c <> "))") . words <$>)
    <$> inp21Str ("d5.input" <> show source)

-- x <- readInstr
-- print $ length $ filter ( (>1) .length) . group . sort $ concat $ uncurry segment <$> x
