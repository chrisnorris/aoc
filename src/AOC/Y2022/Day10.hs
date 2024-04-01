module AOC.Y2022.Day10 where

import AOC.Y2021.Day24 (integer)
import Control.Arrow ((&&&))
import Data.Set (fromList, size)
import Library hiding (fromList, size)
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim

data Terminal where
  Add :: Integer -> Terminal
  Noop :: Terminal

main_pt1 = do
  input <- inpStr 2022 "d10.input"
  let t = sequence (parse commands "motions" <$> input) ^. _Right
      cycles = take 6 $ unfoldr (Just . (id &&& (+ 40))) 20
      res = (cycl 1 1 [] t !!) . subtract 1 <$> cycles
  return $ foldr (\(a, b) x -> x + a * (b + 1)) 0 res

main_pt2 = do
  input <- inpStr 2022 "d10.input"
  let t = sequence (parse commands "motions" <$> input) ^. _Right
  let aa = [(1, 0)] <> cycl 1 1 [] t
  let checkpos i pos = if abs (i - pos) <= 1 then '#' else '.'
  return
    ( ( \i ->
          let [pos, pos2] = fst . getAt aa <$> [i, i + 1]
           in [(pos, mod i 40, checkpos (mod i 40) pos), (pos2, mod (i + 1) 40, checkpos (mod (i + 1) 40) pos2)]
      )
        <$> [x * 2 | x <- [0 .. 119]]
    )

main_p2p = do
  bb <- main_pt2
  let result = (\(_, _, i) -> i) <$> concat bb
  let answer acc [] = acc
      answer acc l = answer (acc <> [[take 40 l]]) (drop 40 l)
  mapM_ print (answer [] result)

getAt aa i = head $ filter (\(_, y) -> y == i) aa

cycl _ _ acc [] = (1, 1) : reverse acc
cycl x c acc (Noop : rest) = cycl x (c + 1) ((x, c) : acc) rest
cycl x c acc (Add i : rest) = cycl (x + i) (c + 2) ([(x + i, c + 1), (x, c)] <> acc) rest

commands = try noop <|> try addd

noop = do
  string "noop"
  return Noop

addd = do
  string "addx"
  many1 space
  sgn <- many (char '-')
  i <- integer
  return $ Add (if sgn == "-" then -i else i)
