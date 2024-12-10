module AOC.Y2024.Day10 where

import qualified Data.Set as S
import Library

main_pt1 = do
  (m, d, f) <- getInputs
  return $ sum $ findAll '0' m d <&> \e -> length $ S.fromList $ loopy 0 (f e) f

main_pt2 = do
  (m, d, f) <- getInputs
  return $ length $ concat $ loop 0 (f <$> findAll '0' m d) f

getInputs = do
  m <- lines <$> parseLists
  let d = length m
  let f = ((\(t, v) -> findOne t v m d) <$>) . snd
  return (m, d, f)

test_pt1_2 =
  liftM2 (,) main_pt1 main_pt2
    >>= return . (== (674, 1372))

p m (x, y) = (m !! x) !! y

bounded c d = (c > (-1)) && (c < d)

v (a, b) d c m =
  [ pp
    | x <- [-1, 0, 1],
      y <- [-1, 0, 1],
      abs x /= abs y,
      bounded (a + x) d,
      bounded (b + y) d,
      let pp = (a + x, b + y),
      p m pp == succ c
  ]

findAll c m d =
  [ ((p m (x, y), (x, y)), (\a -> (a, p m a)) <$> v (x, y) d c m)
    | x <- [0 .. d - 1],
      y <- [0 .. d - 1],
      p m (x, y) == c
  ]

findOne (x, y) c m d = ((p m (x, y), (x, y)), [(e, p m e) | e <- v (x, y) d c m])

parseLists = readFileY 2024 "d10.input"

loopy n l f
  | n == 8 = l
  | otherwise = loopy (n + 1) (f `concatMap` l) f

loop n l f
  | n == 8 = l
  | otherwise = loop (n + 1) (f <$> concat l) f
