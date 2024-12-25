module AOC.Y2024.Day19 where

import AOC.Y2021.Day24 (integer)
import Data.List
import Data.Map qualified as M
import Library

main_pt1 = do
  m <- readFileY 2024 "d19.input.sam"
  let (Right Onsen{..}) = parse o "" m
      p = permutations availableTowels
  
      (q, q2) = (take 63 p, take 128 $ drop 63 p)
      m1 = m2 q toDisplay M.empty
      mn =
           foldl' (\ mp qs -> m2 qs toDisplay mp) m1 (chunksOf 64 q2)
  
  return $ mn

  -- forM_ toDisplay $ \x -> print $ d2 q x
  
      -- d t =
      --   let (Right a) = sequence $ (\y -> parse (many $ r y) "" t) <$> p in
      --   (t, t `notElem` (concat <$> a))

  -- return $ xx -- d <$> toDisplay

main_pt2 = undefined

m2 q [] mm = mm 
m2 q (x:xs) mm = 
   let (k,v) = d2 q x in  
   m2 q xs (M.insert k v mm)

d2 [] t = (t, False)
d2 (y:ys) t =
        let (Right a) = ( parse (many $ r y) "" t) in
        if t == (concat a) then (t,True) else d2 ys t

data Onsen = Onsen {
  availableTowels :: [String],
  toDisplay :: [String]
} deriving Show

o = Onsen <$> avP <*> toDP

avP = (many space >> many1 letter) `sepBy` char ','
       <* many1 (char '\n')
      
toDP = many1 letter `sepBy` char '\n'

r (i:is) = foldl' (\b a  -> b <|> try (string a)) (try $ string i) is

rotate' :: Int -> [a] -> [a]
rotate' _ [] = []
rotate' n xs = zipWith const (drop n (cycle xs)) xs
