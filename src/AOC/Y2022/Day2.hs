module AOC.Y2022.Day2 where

import Library
import Data.List


data Opp = A | B | C deriving (Read, Show)

data Me = X | Y | Z  deriving (Read, Show, Enum)

data Outcome = Win | Draw | Lose deriving (Read, Show)

type GamePlay = (Opp, Me) -> (Int, Outcome)

main_pt1 = run play

main_pt2 = run playToOutcome

run :: GamePlay -> IO Int
run f =
     getGames >>=
      return . sum . (score . f <$>) 
 where score = uncurry (+) . fmap outcome

play :: GamePlay
play = \case
  (A, X) -> (score X , Draw)
  (A, Y) -> (score Y, Win)
  (A, Z) -> (score Z, Lose)
  (B, X) -> (score X, Lose)
  (B, Y) -> (score Y, Draw)
  (B, Z) -> (score Z, Win)
  (C, X) -> (score X, Win)
  (C, Y) -> (score Y, Lose)
  (C, Z) -> (score Z, Draw)

playToOutcome :: GamePlay
playToOutcome = \case
  (A, Y) -> (score X , Draw)
  (A, Z) -> (score Y, Win)
  (A, X) -> (score Z, Lose)
  (B, X) -> (score X, Lose)
  (B, Y) -> (score Y, Draw)
  (B, Z) -> (score Z, Win)
  (C, Z) -> (score X, Win)
  (C, X) -> (score Y, Lose)
  (C, Y) -> (score Z, Draw)

getGames :: IO [(Opp, Me)]
getGames= do
  games <- map parseAsGame <$> inpStr 2022 "d2.input"
  return $ toGameTuple <$> games

parseAsGame = fmap (drop 1) . break ( == ' ')

score = (+ 1) . fromEnum

toGameTuple = bimap read read

outcome :: Outcome -> Int
outcome Win = 6
outcome Lose = 0
outcome Draw = 3