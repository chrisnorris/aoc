module AOC.Y2022.Day9 where

import           AOC.Y2021.Day24                ( integer )
import           Library                 hiding ( size
                                                , fromList
                                                )

import           Data.Set                       ( size
                                                , fromList
                                                )
import           Data.Tuple                     ( swap )
import           Data.Function                  ( (&) )
import           Text.Parsec
import           Text.Parsec.Prim
import           Text.Parsec.Char
import           Text.Parsec.Combinator


data Inst = I Char Integer
  deriving (Eq, Show)

main_pt1 = do
  input <- inpStr 2022 "d9.input"
  sequence (parse commands "motions" <$> input)
    ^. _Right
    &  atLeastOnce
    .  swingRope
    .  instructions
 where
  commands    = try (I <$> choice (char <$> "RULD") <*> integer)
  atLeastOnce = return . size . fromList
  swingRope   = foldl (\ac v -> ac <> [tailPos v $ last ac]) [(0, 0)]

main_pt2 = undefined

tailPos (a, b) (c, d) =
  let tgt    = [ (x, y) | y <- [-2, 2], x <- [-1 .. 1] ]
      shft   = replicate 3 (0, -1) <> replicate 3 (0, 1)
      horizs = tgt `zip` shft
      verts  = (swap <$> tgt) `zip` (replicate 3 (-1, 0) <> replicate 3 (1, 0))
  in  case lookup (a - c, b - d) (horizs <> verts) of
        Just (x, y) -> (a - x, b - y)
        Nothing     -> (c, d)

instructions = foldl
  (\ac (I d s) ->
    let lastPoint@(lx, ly) = last ac
        gen a b = a . b <$> [1 .. s]
    in  case d of
          'R' -> ac <> gen (, ly) (+ lx)
          'L' -> ac <> gen (, ly) (`subtract` lx)
          'U' -> ac <> gen (lx, ) (+ ly)
          'D' -> ac <> gen (lx, ) (`subtract` ly)
  )
  [(0, 0)]
