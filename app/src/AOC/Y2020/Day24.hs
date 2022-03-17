module AOC.Y2020.Day24 where

import Library
import qualified Data.Map as Map

main = do
  source <-
    (below _Right `toListOf`) . (parse gram "" <$>) . lines <$> readFile20
      "d24.input"
  let boundaries = foldl
        (\(x1, y1) e -> case Map.lookup e mp of
          Just (x, y) -> (x + x1, y + y1)
          Nothing     -> (x1, y1)
        )
        (0, 0)
       where
        mp = Map.fromList
          $ zip hexDirs [(0, 1), (1, 0), (1, -1), (0, -1), (-1, 0), (-1, 1)]
  let sourceDirs = boundaries <$> concat source

  return
    $        ((1 ==) . (`rem` 2) . snd)
    `filter` [ (tile, length $ filter (== tile) sourceDirs)
             | tile <- nub sourceDirs
             ]
 where
  hexDirs   = concat $ words <$> ["e se sw w nw ne"]
  gram      = many $ parseDirs hexDirs
  parseDirs = ap (foldr chainTry . try . string . head) tail
  chainTry  = (<|>) . try . string
