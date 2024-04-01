module AOC.Y2022.Day9 where

import AOC.Y2021.Day24 (integer)
import Data.Function ((&))
import Data.Set
  ( fromList,
    size,
  )
import Data.Tuple (swap)
import Library hiding
  ( fromList,
    size,
  )
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim

data Inst = I Char Integer
  deriving (Eq, Show)

main_pt1 = do
  input <- inpStr 2022 "d9.input"
  sequence (parse commands "motions" <$> input) ^. _Right
    & atLeastOnce
      . swingRope'
      . instructions

commands = try (I <$> choice (char <$> "RULD") <*> integer)

atLeastOnce = return . size . fromList

-- swingRope   = foldl (\ac v -> ac <> [tailPos v $ last ac]) [(0, 0)]
--                      ( b -> a -> b)                       -> b -> t a -> b
swingRope' (a : ta) = foldl (\ac a -> ac <> [tailPos a $ last ac]) [a] ta

swingRope'' ((a, _, _) : ta) = foldl (\ac (a, _, _) -> ac <> [let (x, _, _) = last ac in (tailPos a x, Just x, Just a)]) [(a, Nothing, Nothing)] ta

main_pt2 = do
  input <- inpStr 2022 "d9.input"
  sequence (parse commands "motions" <$> input) ^. _Right
    & atLeastOnce
      . flip (!!) 8
      . iterate swingRope'
      . instructions

tailPos (a, b) (c, d) =
  let tgt = [(x, y) | y <- [-2, 2], x <- [-1 .. 1]]
      shft = replicate 3 (0, -1) <> replicate 3 (0, 1)
      horizs = tgt `zip` shft
      verts = (swap <$> tgt) `zip` (replicate 3 (-1, 0) <> replicate 3 (1, 0))
   in case lookup (a - c, b - d) (horizs <> verts) of
        Just (x, y) -> (a - x, b - y)
        Nothing -> (c, d)

instructions =
  foldl
    ( \ac (I d s) ->
        let lastPoint@(lx, ly) = last ac
            (<+>) a b = ac <> (a . b <$> [1 .. s])
         in case d of
              'R' -> (,ly) <+> (+ lx)
              'L' -> (,ly) <+> (`subtract` lx)
              'U' -> (lx,) <+> (+ ly)
              'D' -> (lx,) <+> (`subtract` ly)
    )
    [(0, 0)]
