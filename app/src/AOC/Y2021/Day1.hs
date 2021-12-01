module AOC.Y2021.Day1 where

import Library

main :: IO Int
main =
  (map read) <$> (lines <$> readFile21 "d1.input") >>= 
    \inp ->
    return . length . filter (== LT) $ uncurry compare <$> inp `zip` (drop 1 inp ++ [0])
