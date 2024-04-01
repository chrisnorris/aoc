module AOC.Y2021.Day21 where

import Library (chunksOf)

main = main21i (4, 8) >>= print >> main21i (8, 9) >>= print

main21i s = do
  let play (p1S, p2S) (s1S, s2S) (d1 : d2 : dice) acc1 acc2
        | s1S >= 1000 =
            let (((_, a), b) : (c, (d, _)) : []) = take 2 acc2 in a * d
        | s2S >= 1000 =
            let (((_, a), b) : (c, (d, _)) : []) = take 2 acc2 in a * d
        | otherwise =
            let x1 = modulo $ p1S + sum d1
                x2 = modulo $ p2S + sum d2
             in play
                  (x1, x2)
                  (s1S + x1, s2S + x2)
                  dice
                  ((x1, x2) : acc1)
                  ( ((s1S + x1, head $ reverse d1), (s2S + x2, head $ reverse d2))
                      : acc2
                  )

  return $ play s (0, 0) (chunksOf 3 $ [1 ..]) [] []
  where
    modulo (snd . flip divMod 10 -> 0) = 10
    modulo (snd . flip divMod 10 -> m) = m
