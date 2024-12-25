module AOC.Y2024.Day15 where

-- this is nearly done for pt 1 - just need to complete the checkMoves and with the list of
-- returned values, put e.g. OOO. to .OOO .

import AOC.Y2021.Day24 (integer)
import Control.Monad.Par hiding (parMap)
import Control.Parallel.Strategies hiding (parMap)
import qualified Data.Map as M
import Library

main_pt1 = do
  m <- readFileY 2024 "d15.input.sam"
  let (Right Warehouse {..}) = parse wh "" m
  let w = Warehouse (warehouse ^.. folded . filtered (/= "")) movements
  return $
    loop mvs w

main_pt2 = undefined

data Warehouse = Warehouse
  { warehouse :: [String],
    movements :: String
  }
  deriving (Show)

loop ns w@Warehouse {..} =
  go warehouse "<^^>" ns (head $ cursor warehouse) []
  where
    go m [] ns s@(x, y) acc = (m, acc)
    go m (mv : mvs) ns s@(x, y) acc =
      let nextLocation = case M.lookup mv ns of
            Just (dx, dy) -> (x - dx, y + dy)
            Nothing -> undefined
          p (x, y) m = (m !! x) !! y
          l = length m - 1
          isInvalid (x, y) = x < 0 || x > l || y < 0 || y > l
          mk l (x, y) c = l & (ix x . ix y) .~ c
       in if isInvalid nextLocation
            then (m, acc)
            else case p nextLocation m of
              '#' -> go m mvs ns s (nextLocation : s : acc)
              '.' -> go (mk (mk m s '.') nextLocation '@') mvs ns nextLocation (nextLocation : s : acc)
              -- 'O' -> (mk m nextLocation '!', checkMove m nextLocation mv []) --debug with the !
              'O' -> (m, checkMove m nextLocation mv []) -- just return the checkMove for now in reality it returns new patched m (L. 54)

checkMove m p@(x, y) '^' acc =
  case (m !! x) !! y of
    'O' -> let q = (x - 1, y) in checkMove m q '^' (p : acc)
    '.' -> acc
    '#' -> []
checkMove m p@(x, y) '>' acc =
  case (m !! x) !! y of
    'O' -> let q = (x, y + 1) in checkMove m q '>' (p : acc)
    '.' -> acc -- patch m for acc O's ?
    '#' -> []

wh = Warehouse <$> warehouseMap <*> movementsList

warehouseMap = many (oneOf "#.O@") `sepBy` char '\n'

movementsList = concat <$> (many (oneOf "<^>v") `sepBy` char '\n')

mvs = M.fromList $ "<v^>" `zip` [(x, y) | y <- [-1, 0, 1], x <- [-1, 0, 1], abs x /= abs y]

p (x, y) m = (m !! x) !! y

cursor l = [(x, y) | let w = length l - 1, x <- [0 .. w], y <- [0 .. w], (l !! x) !! y == '@']
