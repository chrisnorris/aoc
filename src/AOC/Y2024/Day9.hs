module AOC.Y2024.Day9 where

import Data.Vector.Generic.Lens
import qualified Data.Vector.Unboxed as V
import Library

main_pt1 = do
  inp <- ((read . charToString) <$>) <$> readFileY 2024 "d9.input.sam"
  let v = V.foldl' (\(s, acc) a -> (s + 1, replicate a (switchOutput s) : acc)) (0, []) (V.fromList inp)
      x = V.fromList $ reverse $ concat $ concat <$> snd v
      y = moveFileBlocks x
  return $ sum  [e * (read $ (: []) i) |  (i,e) <- (V.toList $ y) `zip` [0..]]

main_pt2 = undefined

moveFileBlocks v =
  let f = V.last v
      d = V.findIndex (== '.') v
   in case d of
        Just i -> moveFileBlocks (V.init $ v & (sliced i 1) .~ (V.fromList [f]))
        Nothing -> v

switchOutput s
  | s `mod` 2 == 0 = show (s `div` 2)
  | otherwise = "."

charToString = (: [])
