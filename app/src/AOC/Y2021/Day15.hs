module AOC.Y2021.Day15 where

import           Library                        ( inp21Str
                                                , forM
                                                , liftM2
                                                , Source(..)
                                                )
import           Data.Char                      ( digitToInt )
import qualified Data.Array                    as A
import           AOC.Y2021.Dijkstra

main = main15i Full >>= print -- >> main10ii Full >>= print

main15i source = do

  (xd, yd)   <- liftM2 (,) length (length . head) <$> getInput source

  inputArray <-
    A.listArray ((0, 0), (xd - 1, yd - 1))
      <$> (map digitToInt . concat <$> getInput source)

  let
    valid (x, y) = x >= 0 && y >= 0 && x < xd && y < yd
    adjacents (x, y) arr =
      [ (ixToNode coords, arr A.! coords)
      | m <- [-1, 0, 1]
      , n <- [-1, 0, 1]
      , let coords = (x + m, y + n)
      , let key = yd * (x + m) + (y + n)
      , valid coords
      , m * m /= n * n
      ]
    ixToNode (x, y) = yd * x + y
    ll =
      [ (ixToNode ix, adjacents ix inputArray)
      | (ix, e) <- A.assocs inputArray
      ]

  let asL :: [((String, String), Float)] =
        (\(n, lofN) -> (\(k, v) -> ((show n, show k), fromIntegral v)) <$> lofN)
          `concatMap` ll
  let g :: Graph = fromList asL
  -- consider that the graph is directed(!)
  
  return $ dijkstra g "9999"

getInput source = inp21Str ("d15.input" <> show source)
