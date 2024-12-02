module AOC.Y2023.Day4 where

import AOC.Y2021.Day24 (integer)
import Library
data ScratchCardWin a = Card a | Winners a [ScratchCardWin a] deriving (Eq, Show, Ord)

main_pt1 = do
  input' <- lines <$> readFileY 2023 "d4.input"
  let readCards = sequence (parse scratchcard "" <$> input') ^. _Right
      scores = (\(i, (winners, haves)) -> (i, score $ length $ winners `intersects` haves)) <$> readCards
  return $ sum $ snd <$> scores

main_pt2 = do
  input' <- lines <$> readFileY 2023 "d4.input"
  let readCards = sequence (parse scratchcard "" <$> input') ^. _Right
      scores = (\(i, (winners, haves)) -> (i, toInteger $ length $ winners `intersects` haves)) <$> readCards
      winners = (\(c, w) -> if w == 0 then Card c else Winners c (Card <$> [c + 1 .. c + 1 + w - 1])) <$> scores
  return $ foldl' convert [] winners

convert acc = \case
  Winners w res -> [Card w] <> res <> (concatMap (\c@(Card x) -> if x == w then [c] <> res else [c]) acc)
  c@(Card _) -> [c] <> acc

scratchcard = do
  string "Card "
  i <- integer
  char ':'
  winners <- numbers
  char '|'
  haves <- numbers
  return (i, (winners, haves))

score 0 = 0
score n = 2 ^ (n - 1)

numbers = concat <$> do many1 integer `sepBy` space

intersects :: [Integer] -> [Integer] -> [Integer]
intersects [] _ = []
intersects _ [] = []
intersects xs ys = filter (`elem` xs) ys
