module AOC.Y2024.Day20 where

import AOC.Y2021.Day24 (integer)
import AOC.Y2024.Day11 (parMap)
import AOC.Y2024.Day16 (cursor, getNextMovesWithAcc, listAll)
import Control.Monad (join)
import Control.Monad.Par hiding (parMap)
import Control.Parallel.Strategies hiding (parMap)
import qualified Data.Set as S
import Library hiding (deep)

main_pt1 = do
  m <- readFileY 2024 "d20.input"
  let x = lines m
      s = head $ cursor x 'S'
      chks = chunksOf 64 (cheats x)
  l <- listAll x s [] []
  let z = (length . concat . concat) l
  a <-
    runEvalIO $
      parMap
        ( \chk ->
            [ z - minimum (length <$> concat l) | (cx, cy) <- chk, let p = x & (ix cx . ix cy) .~ '.', l <- listAll p s [] []
            ]
        )
        chks
        >>= return . (liftM2 (,) head length <$>) . group . sort . join

  return $ sum $ snd <$> filter ((>= 100) . fst) a

main_pt2 = undefined

cheats l =
  [(x, y) | let w = length l - 1, x <- [1 .. w - 1], y <- [1 .. w - 1], (l !! x) !! y == '#']
