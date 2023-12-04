module AOC.Y2023.Day4 where

import AOC.Y2021.Day24(integer)
import Library

main_pt1 = do
  input' <- lines <$> readFileY 2023 "d4.input"
  let readCards = sequence (parse acard "" <$> input') ^. _Right
      scores = (\(i, (winners, haves)) -> (i, score $ length $ winners `intersects` haves)) <$> readCards
  return $ sum $ snd <$> scores

acard = do 
    string "Card "
    i <- integer
    char ':'
    winners <- numbers
    char '|'
    haves <- numbers
    return (i, (winners, haves))

score 0 = 0
score n = 2^(n-1)

numbers = concat <$> do {many1  integer `sepBy` space}

intersects :: [Integer] -> [Integer] -> [Integer]
intersects [] _ = []
intersects _ [] = []
intersects xs ys = filter (`elem` xs) ys
