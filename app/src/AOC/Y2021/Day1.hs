module AOC.Y2021.Day1 where

import Library

main :: IO (Int, Int)
main = do
  let m inp = length . filter (== LT) $ uncurry compare <$> inp `zip` (drop 1 inp <> [0])
  map read <$> (lines <$> readFile21 "d1.input") >>= return . liftM2 (,) m (m . (sum <$>) . win)

win = go []
 where go l [] = l
       go l xs = go (l <> [take 3 xs]) (drop 1 xs)
