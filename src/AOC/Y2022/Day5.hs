module AOC.Y2022.Day5 where

import           Library

import           Data.List
import           Text.Parsec
import           Text.Parsec.Prim
import           Text.Parsec.Char
import           Text.Parsec.Combinator

import           AOC.Y2021.Day24                ( integer )

data Move a = Move
  { pos  :: a
  , from :: a
  , to   :: a
  }
  deriving (Show, Functor)

data Inputs = Inputs
  { machines :: [[String]]
  , moves    :: [Move Int]
  }
  deriving Show

-- DHBJQJCCW
main_pt1 = do
  Inputs{..} <- getParsedInput
  return $ head <$> foldl (makeAMove reverse) machines moves

-- WJVRLSJJT
main_pt2 = do
  Inputs{..} <- getParsedInput
  return $ head <$> foldl (makeAMove id) machines moves

getParsedInput = do
  x <- inpStr 2022 "d5.input"
  let m = rparse instruction (take 8 x)
  return $ Inputs (trimmed' <$> transpose m) (rparse move (drop 10 x))
  
 where bracketedUpper = do {char '['; y <-  upper; char ']'; return [y]}
       trimmed' = dropWhile (== "   ")
       seperator = string $ replicate 3 ' '
       entry = choice [bracketedUpper, seperator]
       instruction = sepEndBy entry space
       rparse i o = resolveM $ parse i "" <$> o
       move = do
          string "move"; space; p <- integer; string "from"
          f <- integer; string "to"; t <- integer
          return $ fromInteger <$> Move p f t

resolveM x =
  case sequence x of 
    Right machineSetUp -> machineSetUp
    Left e -> error $ show e

makeAMove f machine Move {..} =
  let src = machine ^. ix (from - 1)
      dest = f (take pos src) <> (machine ^. ix (to - 1)) in
  machine & ix (from - 1) .~ drop pos src & ix (to - 1) .~ dest
