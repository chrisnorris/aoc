module AOC.Y2021.Day1 where

import Library

main :: IO (Int, Int)
main = do
  let m inp = sum [1 | (f,s) <- inp `zip` tail inp , f < s ]
  liftM2 (,) m (m . (sum <$>) . win []) <$> inp21I "d1.input"
 where 
  inp21I = (Prelude.map read <$>) . inp21Str

win l [] = l
win l xs = win (l <> [take 3 xs]) (drop 1 xs)

