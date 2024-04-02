module AOC.Y2023.Day8 where

import AOC.Y2021.Day24 (integer)
import Data.Char
import Data.List
import qualified Data.Map as M
import Library

main_pt1 = do
  al <- lines <$> readFileY 2023 "d8.input"
  let (Right tuples) = sequence $ parse tuplesParser "" <$> drop 2 al
  return $ loop (concat $ repeat $ head al) 0 "AAA" (M.fromList tuples)
  where
    loop _ n "ZZZ" _ = n
    loop (i : is) n e m =
      let r = let result = fromJust $ M.lookup e m in if i == 'L' then head result else result !! 1
       in loop is (n + 1) r m

main_pt2 = do
  al <- lines <$> readFileY 2023 "d8.input"
  let (Right tuples) = sequence $ parse tuplesParser "" <$> drop 2 al
      m = M.fromList tuples
      starts = filter ( (=='A') . last ) (M.keys m)
  return $ loop (concat $ repeat $ head al) 0 starts (M.fromList tuples)
  where
    loop _ n l _ | (finished l) = n
    loop (i : is) n l m =
      loop is (n + 1) ((\e -> let result = fromJust $ M.lookup e m in if i == 'L' then head result else result !! 1) <$> l) m

tuplesParser = do
  k <- many1 upper
  many (noneOf "(")
  t <- between (char '(') (char ')') (many1 (upper <|> space) `sepBy` char ',')
  return (k, trim <$> t)

trim = dropWhileEnd isSpace . dropWhile isSpace

finished = all ((== 'Z') . last)
