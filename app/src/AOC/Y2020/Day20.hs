module AOC.Y2020.Day20 where

import Library

-- main = do
--     tiles <- parseTiles <$>  readFile20 "input20-ex.txt"
--     tileDef = P.many $ P.oneOf ['#', '.']
--     tileId =  P.string "Tile " >> P.many P.digit
--     let valid C{..} = rot==180
--     let s = [1951,2311,3079,2729,1427,2473,2971,1489,1171]
--     x <- lines <$> readFile20 "input20-ex.txt"
--     A.listArray ((0,0) ,(8,8)) $ unwords $ take 9 . drop 1 $ x
--     put into map of id array (9,9) Int
--     (\x -> [C a b c | a<-["90","180"], b <- [True, False], c <- x, valid (C a b c )] ) <$> (permutations s)

main = undefined