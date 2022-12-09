module AOC.Y2020.Day22 where

import Library


main :: IO Int
main = do
  let game =
        Game
          { player1 = [39, 15, 13, 23, 12, 49, 36, 44, 8, 21, 28, 37, 40, 42, 6, 47, 2, 38, 18, 31, 20, 10, 16, 43, 5],
            player2 = [29, 26, 19, 35, 34, 4, 41, 11, 3, 50, 33, 22, 48, 7, 17, 32, 27, 45, 46, 9, 25, 30, 1, 24, 14]
          }

  let gamesamp = Game {player1 = [9, 2, 6, 3, 1], player2 = [5, 8, 4, 7, 10]}

  return $ round game
  where
    round (Game [] p2) = computeScore p2
    round (Game p1 []) = computeScore p1
    round
      (Game player1@(p : ps) player2@(q : qs)) =
        case p `compare` q of
          GT -> round (Game (ps <> [p, q]) qs)
          LT -> round (Game ps (qs <> [q, p]))
          _ -> error "cards cannot be same"
    computeScore hand = sum $ zipWith (*) (reverse hand) [1 ..]

data Game = Game {player1 :: [Int], player2 :: [Int]} deriving (Eq, Show)
