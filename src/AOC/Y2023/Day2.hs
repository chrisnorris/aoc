module AOC.Y2023.Day2 where


import Data.Char
import AOC.Y2021.Day24(integer)
import Data.Map as Map
import Data.Tuple
import Library

data Game = Game {
  gid :: Integer,
  games :: [GameSet]
} deriving (Eq, Show)

newtype GameSet = GameSet{oneGame :: [(Integer, Color)]} deriving (Eq, Show)
                  
data Color = Red | Green | Blue deriving (Eq, Show, Read, Enum, Ord)

main_pt1 = do
  input' <- lines <$> readFileY 2023 "d2.input"
  let tryGames = parse (Game <$> gidP <*> gamesP) "" <$> input'
      games = sequence tryGames ^. _Right
  return $ sum $ fst <$> Prelude.filter (all (==True) . snd) (passOrFailGame <$> games)

main_pt2 = do
  input' <- lines <$> readFileY 2023 "d2.input"
  let tryGames = parse (Game <$> gidP <*> gamesP) "" <$> input'
      games = sequence tryGames ^. _Right
  return $ sum
    (Map.foldl (\ i b -> i * maximum b) 1 . Map.fromListWith (<>) <$>
     (flattenGames <$> games))

passOrFailGame (gm :: Game) =
  (gid gm,[ check (Map.fromList $ swap <$> oneGame g) | (g :: GameSet ) <- games gm ])

flattenGames gm = concat [ [(b, [a]) | (a, b) <- oneGame g] | g <- games gm ]

check x = all (==True) ((\(c,i) -> case  Map.lookup c x of Just x -> x <= i ; Nothing -> True) <$> gameBag)
gameBag = (toEnum <$> [0..]) `zip` [12..14]

gidP = do string "Game";  i <- integer; string ":"; space; return i

colorPair = liftM2 (,) integer 
  (do c <- many1 (letter <|> noneOf ",;"); return $ read (c & ix 0 %~ toUpper))

oneGameP = GameSet <$> (concat <$> (many1  colorPair `sepBy` string ","))

gamesP = oneGameP `sepBy` string ";"
