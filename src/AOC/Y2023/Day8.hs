module AOC.Y2023.Day8 where

import AOC.Y2021.Day24(integer)
import Library
import Data.Map qualified as M
import Data.List
import Data.Char

main_pt1 = do
  al <- lines <$> readFileY 2023 "d8.input"
  let (Right tuples) = sequence $ parse (tuplesParser) "" <$> (drop 2 al)
  return $ loop (concat $ repeat $ head al) 0 "AAA" (M.fromList tuples)

 where  loop _ n "ZZZ" _ = n
        loop (i:is) n e m =
              let r = let result = fromJust $ M.lookup e m in if i == 'L' then (head result) else (head $ drop 1 $ result) in
              loop is (n+1) r m

tuplesParser = do 
  k <- many1 upper; many (noneOf "(")
  t <- between (char '(') (char ')') ( many1 (upper <|> space ) `sepBy` (char ','))
  return (k, trim <$> t)

trim = dropWhileEnd isSpace . dropWhile isSpace