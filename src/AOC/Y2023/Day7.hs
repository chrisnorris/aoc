module AOC.Y2023.Day7 where

import AOC.Y2021.Day24 (integer)
import Control.Arrow ((&&&))
import Data.List (sortOn)
import Data.Maybe
import Data.Ord (comparing)
import Library

main_pt1 = main handType
main_pt2 = main handType2

data CamelCards a = CamelCards
  { hand :: a,
    bid :: Integer
  } deriving (Show, Functor)

data Hand a = HighCard a | OnePair a | TwoPair a | ThreeOfAKind a | FullHouse a | FourOfAKind a | FiveOfAKind a
  deriving (Show, Eq, Ord, Functor)

data Rank a = C1 a | T | J | Q | K | A deriving (Eq, Ord, Show)

camelCardsParser = 
  CamelCards
  <$> do x <- many1 (digit <|> upper); spaces; return x
  <*> integer

readCards = ((sequence <$>) . pure . ( parse camelCardsParser "" <$>)) =<< (lines <$> readFileY 2023 "d7.input")

main h = do
  (Right p) <- readCards
  return $ sum (results h p)

results h p =  zipWith (\i CamelCards {..} -> i * bid) [1 ..] (sortOn hand $ (h <$>) <$> p)
handType = uncurry (flip rankCard) . ((toRank <$>) &&& (group . sort))

toRank 'T' = T
toRank 'J' = J
toRank 'Q' = Q
toRank 'K' = K
toRank 'A' = A
toRank d = C1 d

toRank2 J = C1 '1'
toRank2 x = x

crumple :: (Eq a) => [[a]] -> [(Int, [a])]
crumple = sortOn fst <$> map (length &&& nub)

swapJ _ acc [] = handType acc
swapJ c acc (s:st) = 
  case s of 
   'J' -> swapJ c ( c:acc) st
   _   -> swapJ c ( s:acc) st

handType2 card =
  let newHandType = maximum [swapJ c [] card | c <- "TJQKA123456789"]
  in (toRank2 . toRank <$> card) <$ newHandType

rankCard (crumple -> [(5, s)]) = FiveOfAKind
rankCard (crumple -> [(1, s), (4, s')]) = FourOfAKind
rankCard (crumple -> [(2, s), (3, s')]) = FullHouse
rankCard (crumple -> [(1, s), (1, s'), (3, s'')]) = ThreeOfAKind
rankCard (crumple -> [(1, s), (2, s'), (2, s'')]) = TwoPair
rankCard (crumple -> [(1, s), (1, s'), (1, s''), (2, _)]) = OnePair
rankCard (crumple -> [(1, s), (1, s'), (1, s''), (1, t), (1, t')]) = HighCard
