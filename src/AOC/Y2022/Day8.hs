module AOC.Y2022.Day8 where

import           Library                        ( inpStr
                                                , intersperse
                                                )

import           Data.List                      ( transpose )

data Visibles a = V
  { l :: a
  , r :: a
  , u :: a
  , d :: a
  }
  deriving (Eq, Show, Functor)

main_pt1 = do
  ((dx, dy), patch) <- getParsedInput
  return $ 2 * (dx + dy - 2) + sum
    (   allClear
    <$> [ all (< lookupList t patch) <$> asDirns patch t
        | t <- [ (x, y) | x <- [2 .. dx - 1], y <- [2 .. dy - 1] ]
        ]
    )

main_pt2 = do
  ((dx, dy), patch) <- getParsedInput
  let dropZeros V {..} = dropWhile (< 1) <$> [reverse l, r, reverse u, d]
  return $ maximum
    [ let f = lookupList t patch
      in  product $ lengthUntil f <$> dropZeros (asDirns patch t)
    | t <- [ (x, y) | x <- [1 .. dx], y <- [1 .. dy] ]
    ]

asDirns patch (b, a) =
  let v = patch !! (b - 1)
      h = transpose patch !! (a - 1)
  in  V (take (a - 1) v) (drop a v) (take (b - 1) h) (drop b h)

lengthUntil x =
  (\case
      (xs, []) -> length xs
      (xs, _ ) -> 1 + length xs
    )
    . break (>= x)

getParsedInput = do
  x <- fmap (fmap read . words . intersperse ' ') <$> inpStr 2022 "d8.input"
  let dims = (,) (length x) (length (head x))
  return (dims, x)

lookupList (a, b) ll = (ll !!) (a - 1) !! (b - 1)

allClear V {..} = fromEnum $ or [l, r, u, d]
