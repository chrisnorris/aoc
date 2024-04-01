module AOC.Y2021.Day11 where

import qualified Data.Array as A
import Data.Char (digitToInt)
import Library

main = main11 Sample >>= print >> main11 Full >>= print

main11 source = do
  (xd, yd) <- liftM2 (,) length (length . head) <$> getInput source
  inputArray <-
    A.listArray ((0, 0), (xd - 1, yd - 1))
      <$> (map digitToInt . concat <$> getInput source)

  let valid (x, y) = x >= 0 && y >= 0 && x < xd && y < yd
      adjacents arr (x, y) =
        [ (coords, arr A.! coords + 1)
          | m <- [-1, 0, 1],
            n <- [-1, 0, 1],
            let coords = (x + m, y + n),
            valid coords,
            (m, n) /= (0, 0)
        ]
  let synced input = all (== 0) [e | (ix, e) <- A.assocs input]
  let nines input =
        concat $
          (\(ix, _) -> (ix, 0) : adjacents input ix)
            <$> take
              1
              [r | r@(_, e) <- A.assocs input, e > 9]

  let aux input flashes =
        let overNinesWith f =
              f `concatMap` take 1 [r | r@(_, e) <- A.assocs input, e > 9]
            ninesAdj = overNinesWith (adjacents input . fst)
            flasheds = overNinesWith ((: flashes) . (,0) . fst)
         in if null ninesAdj
              then (length flashes, input)
              else aux ((A.//) input (ninesAdj <> flasheds)) flasheds

  let run n input acc
        | n > 0 =
            let (flashes, input') = aux ((+ 1) <$> input) []
             in run (n - 1) input' (acc + flashes)
        | otherwise =
            acc

  let runUntil input n
        | synced input =
            n
        | otherwise =
            let (flashes, input') = aux ((+ 1) <$> input) []
             in runUntil input' (n + 1)

  -- part i
  -- return $ run 100 inputArray 0 -- 1656, 1702

  -- part ii
  return $ runUntil inputArray 0 -- 195, 251
  where
    getInput source = inp21Str ("d11.input" <> show source)
