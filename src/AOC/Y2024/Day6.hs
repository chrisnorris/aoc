module AOC.Y2024.Day6 where

import AOC.Y2021.Day24 (integer)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Tuple
import Library

main_pt1 = do
  m <- lines <$> parseLists
  return $
    (S.size . S.fromList) $
      loop
        (patchCursor m)
        (M.fromList $ "<v^>" `zip` [(x, y) | x <- [-1, 0, 1], y <- [-1, 0, 1], abs x /= abs y])
        (head $ cursor m)

main_pt2 = do
  m <- lines <$> parseLists
  return $
    ( loop2
        (M.fromList $ "<v^>" `zip` [(x, y) | x <- [-1, 0, 1], y <- [-1, 0, 1], abs x /= abs y])
        (head $ cursor m)
    )
      <$> (obstructAllPositions . patchCursor $ m)

parseLists = readFileY 2024 "d6.input"

loop2 ns s m =
  go m ns s 0 '^' []
  where
    go m ns s@(x, y) dist c acc =
      let nLoc = case M.lookup c ns of
            Just (dy, dx) -> (x - dx, y + dy)
            Nothing -> undefined
          p (x, y) m = (m !! x) !! y
          l = length m - 1
          isInvalid (x, y) = x < 0 || x > l || y < 0 || y > l
       in if (isInvalid nLoc)
            then 0
            else
              if ((detectCycles acc))
                then 1
                else case p nLoc m of
                  '#' -> go m ns s dist (rotateC c) acc
                  '.' -> go m ns nLoc (dist + 1) c ((c, nLoc) : acc)

detectCycles l = case ((length l) `compare` 4) of
  GT ->
    let dc = detectCycle l '^' []
     in if (rem (length dc) 4) == 0 then (any (> 1) $ length <$> (group $ sort (chunksOf 4 $ snd <$> dc))) else False
  _ -> False
  where
    detectCycle [] c acc = acc
    detectCycle (l : ls) c acc
      | (fst l) == c = detectCycle ls c acc
      | (fst l) /= c = detectCycle ls (fst l) (l : acc)

loop m ns s =
  go m ns s 0 '^' []
  where
    go m ns s@(x, y) dist c acc =
      let nLoc = case M.lookup c ns of
            Just (dy, dx) -> (x - dx, y + dy)
            Nothing -> undefined
          p (x, y) m = (m !! x) !! y
          l = length m - 1
          isInvalid (x, y) = x < 0 || x > l || y < 0 || y > l
       in if isInvalid nLoc
            then acc
            else case p nLoc m of
              '#' -> go m ns s dist (rotateC c) acc
              '.' -> go m ns nLoc (dist + 1) c (nLoc : acc)

rotateC '^' = '>'
rotateC '>' = 'v'
rotateC 'v' = '<'
rotateC '<' = '^'

cursor l = [(x, y) | let w = length l - 1, x <- [0 .. w], y <- [0 .. w], (l !! x) !! y == '^']

obstructAllPositions l = [(l & (ix x . ix y) .~ '#') | let w = length l - 1, x <- [0 .. w], y <- [0 .. w], (l !! x) !! y == '.']

patchCursor l =
  let (cx, cy) = (head $ cursor l)
   in l & (ix cx . ix cy) .~ '.'
