module AOC.Y2023.Day7 where

import AOC.Y2021.Day24 (integer)
import Control.Arrow((&&&))
import Data.Maybe
import Data.List(sortOn)
import Data.Ord(comparing)
import Library

data CamelCards a = CamelCards {
  hand :: a,
  bid :: Integer
} deriving (Show, Functor)

camelCardsParser = CamelCards <$> hands <*> integer

main_pt1 = do
  t <- lines <$> readFileY 2023 "d7.input"
  let (Right p) = sequence $ parse camelCardsParser "" <$> t
  let res = zipWith (\i CamelCards{..} -> i * bid) [1..] (sortOn hand $ (handType <$>) <$> p)
  return $ sum res

hands = do {x <- many1 (digit <|> upper); spaces; return x}
handType = uncurry (flip rankCard) . ((toRank <$>) &&& (group . sort))

data Hand a = HighCard a | OnePair a | TwoPair a | ThreeOfAKind a | FullHouse a | FourOfAKind a | FiveOfAKind a
  deriving (Show, Eq, Ord)

data Rank a = C1 a | T | J | Q | K | A deriving (Eq, Ord, Show)

toRank 'T' = T
toRank 'J' = J
toRank 'Q' = Q
toRank 'K' = K
toRank 'A' = A
toRank d = C1 d

rankCard (crumple -> [(5, s)]) = FiveOfAKind
rankCard (crumple -> [(1, s), (4, s')]) = FourOfAKind
rankCard (crumple -> [(2, s), (3, s')]) = FullHouse
rankCard (crumple -> [(1, s), (1,s'), (3, s'')]) = ThreeOfAKind
rankCard (crumple -> [(1, s), (2, s'), (2, s'')]) = TwoPair
rankCard (crumple -> [(1, s), (1, s'), (1, s''), (2, _)]) = OnePair
rankCard (crumple -> [(1, s), (1, s'), (1, s''), (1, t), (1, t')]) = HighCard

crumple :: Eq a => [[a]] -> [(Int, [a])]
crumple =  sortOn fst <$> map (length &&& nub)
