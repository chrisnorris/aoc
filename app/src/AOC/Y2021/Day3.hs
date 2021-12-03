module AOC.Y2021.Day3 where

import Library


main3 :: IO (Int, Int)
main3 = do
   ins <- readInstr
   let m op = reverse $ uncurry ((. length) . op . length) . (bitat ins) <$> [0..11]
   
   return $ (toInt $ m (>), toInt $ m (<))

 where
       readInstr = map words <$> (inp21Str "d3.input")
       bitat x n = partition (== "1") $ (map (!!n)) <$> x
       toInt l = 0 & partsOf (taking 12 bits) .~ l

main = main3 >>= print
