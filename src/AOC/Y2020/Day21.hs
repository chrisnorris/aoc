module AOC.Y2020.Day21 where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Library

main = do
  recipes <- (parse foods "" <$>) . lines <$> readFile20 "d21.input" >>= (\p -> return $ concat $ p ^.. below _Right)
  let allergenMap = foldl go Map.empty recipes
  let dangerousIngredientsList = ["ltbj", "nrfmm", "pvhcsn", "jxbnb", "chpdjkf", "jtqt", "zzkq", "jqnhd"]

  return $ sum $ (\Food {..} -> length $ (not . (`elem` dangerousIngredientsList)) `Prelude.filter` ingredients) <$> recipes
  where
    foods = do
      ingredients <- many $ noneOf ['(']
      Food (words ingredients) <$> between op cl allergenList
    op = char '('
    cl = char ')'
    comma = char ','
    allergenEnd = noneOf [',', ')']
    allergenList = string "contains" >> (space >> many allergenEnd) `sepBy` comma
    go st v = intoMap st (ingredients v) (allergens v)
    intoMap st i = \case
      [] -> st
      allergen : as -> case Map.lookup allergen st of
        Nothing -> intoMap (allergen `Map.insert` Set.fromList i $ st) i as
        Just all -> intoMap (allergen `Map.insert` Set.intersection all (Set.fromList i) $ st) i as

data Food = Food {ingredients :: [String], allergens :: [String]} deriving (Eq, Show, Ord)
