module AOC.Y2024.Day13 where

import AOC.Y2021.Day24 (integer)
import Library

bs (x, y) (as, bs) ba bb =
  let (ex, ey) = (x + as * (dx ba), y + as * (dy ba))
   in (ex + bs * (dx bb), ey + bs * (dy bb))

main_pt1 = do
  input <- readFileY 2024 "d13.input"
  let (Right Game {..}) = (parse games "" input)
  return $ sum $ getPrizes <$> game

getPrizes Prize {..} =
  let r = [(a * 3 + b) | a <- [0 .. 10000000000000], b <- [0 .. 10000000000000], let (px, py) = bs (0, 0) (a, b) buttonA buttonB, px == (xp prizeAm), py == (yp prizeAm)]
   in if null r then 0 else minimum r

prizesP = Prize <$> button <*> button <*> prize

games = Game <$> (many1 prizesP)

main_pt2 = undefined

data Button = Button
  { dx :: Integer,
    dy :: Integer
  }
  deriving (Eq, Show)

data PrizeAmounts = PrizeAmounts
  { xp :: Integer,
    yp :: Integer
  }
  deriving (Eq, Show)

data Prize = Prize
  { buttonA :: Button,
    buttonB :: Button,
    prizeAm :: PrizeAmounts
  }
  deriving (Eq, Show)

data Game = Game
  { game :: [Prize]
  }
  deriving (Eq, Show)

button = do
  string "Button"
  spaces
  l <- oneOf "AB"
  char ':'
  s1 <- summy '+'
  char ','
  s2 <- summy '+'
  return (Button s1 s2)

summy c = do
  spaces
  xy <- oneOf "XY"
  char c
  i <- integer
  return i

prize = do
  string "Prize:"
  spaces
  s1 <- summy '='
  char ','
  s2 <- summy '='
  return (PrizeAmounts (s1 + 10000000000000) (s2 + 10000000000000))
