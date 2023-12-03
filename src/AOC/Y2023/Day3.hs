module AOC.Y2023.Day3 where

import AOC.Y2021.Day24(integer)

import qualified Data.Array as A
import Data.Char

import Library

main_pt1 :: IO Integer
main_pt1 = do
  input' <- readFileY 2023 "d3.input"
  let input = concat . lines $ input'
      l = length $ lines input'
      aa = A.listArray ((0, 0), (l-1, l-1)) input
      stuff =  (\ (t, e) -> (e, adjacents t l aa) ) <$> A.assocs aa
      f = span (isDigit . fst)
  return $
         sum ( 
            read <$>
               filter (/= "")
                 ((\ e ->
                     if all (\ x -> x == '.' || isDigit x) (concat $ snd <$> e) then
                       [] else fst <$> e )
                    <$> tidy (span (isDigit . fst)) [] (f stuff))
             )
      
  where 
        adjacents (x, y) l arr =
         [ arr A.! (x + m, y + n)
           | m <- [-1, 0, 1],
             n <- [-1, 0, 1],
             (m, n) /= (0, 0),
             x+m > -1, y+n > -1, x+m < l, y+n < l
         ]
        tidy f acc (res, []) = acc <> [res]
        tidy f acc (res, rest) = tidy f (acc <> [res]) (f $ drop 1 rest)

main_pt2 = undefined
