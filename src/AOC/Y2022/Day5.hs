module AOC.Y2022.Day5 where

import           Library

import           Data.List
import           Text.Parsec
import           Text.Parsec.Prim
import           Text.Parsec.Char
import           Text.Parsec.Combinator

import           AOC.Y2021.Day24                ( integer )

data Move a = Move { pos :: a, from :: a, to :: a } deriving (Show, Functor)
data Inputs = Inputs { machines :: [[String]] , moves :: [Move Int] } deriving Show

-- DHBJQJCCW
main_pt1 = do
  Inputs{..} <- getParsedInput
  return $ head <$> foldl (go reverse) machines moves

-- WJVRLSJJT
main_pt2 = do
  Inputs{..} <- getParsedInput
  return $ head <$> foldl (go id) machines moves

getParsedInput = do
  (model, moves) <- bimap init tail . break (== mempty) <$> inpStr 2022 "d5.input"
  let instrns = parse' instruction model
  return $ Inputs (dropWhile (== seperator) <$> transpose instrns) (parse' move moves)
  
 where bracketedUpper = do {char '['; y <-  upper; char ']'; return [y]}
       seperator = "   "
       entry = choice [bracketedUpper, string seperator]
       instruction = sepEndBy entry space
       parse' i o = sequence (parse i "" <$> o) ^._Right
       move = do
          string "move"; space; p <- integer; string "from"
          f <- integer; string "to"; t <- integer
          return $ fromInteger <$> Move p f t

go f machine Move {..} =
  let src  = machine ^. ix (from - 1)
      dest = f (take pos src) <> (machine ^. ix (to - 1))
  in  machine & ix (from - 1) .~ drop pos src & ix (to - 1) .~ dest
