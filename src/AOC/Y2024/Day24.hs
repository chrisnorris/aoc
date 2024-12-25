module AOC.Y2024.Day24 where

import AOC.Y2021.Day24 (integer)
import Data.Char (digitToInt)
import Data.List.Split
import qualified Data.Map as M
import Library

main_pt1 = do
  l <- lines <$> readFileY 2024 "d24.input"
  let a : as = whenElt null `split` l
      (Right m) = sequence (parse wvs "" <$> a)
      (Right n) = sequence (parse gp "" <$> concat (drop 1 as))
      g = Game m n
      iwvMap = M.fromList $ (\IWV {..} -> (wire, wireValue)) <$> iwvs g
      zs = sort $ filter (\(a : as) -> a == 'z') $ result <$> gates g
      fr = doloopy iwvMap g <$> zs
  return $ foldl (\x d -> 2 * x + d) 0 (reverse fr)

main_pt2 = undefined

doloopy iwvMap g b =
  let l = loopy iwvMap (gates g) []
   in case M.lookup b l of
        Just v -> v
        Nothing -> doloopy l g b

loopy i [] acc = i
loopy i (g : gs) acc =
  let Gates {..} = g
   in case M.lookup wire1 i of
        Just x ->
          let z =
                case M.lookup wire2 i of
                  Just zv -> M.insert result (performOp operation x zv) i
                  Nothing -> i
           in loopy z gs ((x, wire1) : acc)
        Nothing -> loopy i gs acc

performOp :: String -> Int -> Int -> Int
performOp "AND" x y = fromEnum $ toEnum x && toEnum y
performOp "OR" x y = fromEnum $ toEnum x || toEnum y
performOp "XOR" x y = x `xor` y

data IWV = IWV
  { wire :: String,
    wireValue :: Int
  }
  deriving (Eq, Show)

data Gates = Gates
  { wire1 :: String,
    wire2 :: String,
    operation :: String,
    result :: String
  }
  deriving (Eq, Show)

data Game = Game
  { iwvs :: [IWV],
    gates :: [Gates]
  }
  deriving (Eq, Show)

wvs :: ParsecT String () Identity IWV
wvs = do
  n <- manyTill anyChar (char ':')
  spaces
  IWV n <$> (fromIntegral <$> integer)

gp = do
  x <- manyTill anyChar space
  op <- manyTill anyChar space
  y <- manyTill anyChar space
  string "->"
  space
  z <- many anyChar
  return $ Gates x y op z
