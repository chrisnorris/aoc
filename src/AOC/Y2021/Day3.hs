module AOC.Y2021.Day3 where

import Library

main3 :: IO (Int, Int)
main3 = do
  ins <- readInstr
  let m op = reverse $ uncurry ((. length) . op . length) . bitat ins <$> [0 .. 11]

  return (toInt $ m (>), toInt $ m (<))
  where
    readInstr = map words <$> inp21Str "d3.input"
    bitat x n = partition (== "1") $ map (!! n) <$> x
    toInt l = 0 & partsOf (taking 12 bits) .~ l

main = main3ii >>= print

main3ii =
  -- 1184 2256
  readInstr
    >>= lifeSupportRating '0' '1'
    >> readInstr
    >>= lifeSupportRating '1' '0'

lifeSupportRating c d ins =
  foldM
    ( \acc i ->
        let (x, zs) = bitat acc i
         in if x >= zs
              then debug (filterWithAtN acc i c) >> return (filterWithAtN acc i c)
              else debug (filterWithAtN acc i d) >> return (filterWithAtN acc i d)
    )
    ins
    [0 .. 11]

readInstr = inp21Str "d3.input"

bitat x n = let (a, b) = partition (== '1') ((!! n) <$> x) in (length a, length b)

filterWithAtN ls n c = [x | x <- ls, x !! n == c]
