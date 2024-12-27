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
  let (Right tuples) = sequence $ parse tuplesParser2 "" <$> drop 2 al
      m = M.fromList tuples
      starts = filter ((== 'A') . last) (M.keys m)
  return $ loop ((take 110000256 $ concat $ repeat $ head al), 0, (M.fromList tuples), starts)
  where
    -- loop _ n _ l | finished l = n
    loop (p@(i : is), n, m, l) =
      let f = finished l
       in if f
            then n
            else
              let pp =
                    ( ( \e ->
                          let (a, b) = fromJust $ M.lookup e m
                           in if i == 'L' then a else b
                      )
                        <$> l
                    )
               in loop (is, (n + 1), m, pp)

tuplesParser = do
  k <- many1 (upper <|> digit)
  many (noneOf "(")
  t <- between (char '(') (char ')') (many1 (upper <|> space <|> digit) `sepBy` char ',')
  return (k, trim <$> t)

tuplesParser2 = do
  k <- many1 (upper <|> digit)
  many (noneOf "(")
  t <- between (char '(') (char ')') (do x <- aunit; char ','; y <- aunit; return (x, y))
  return (k, trim <$> t)

aunit = many1 (upper <|> space <|> digit)

trim = dropWhileEnd isSpace . dropWhile isSpace

finished = all ((== 'Z') . last)
