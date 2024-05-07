module AOC.Y2023.Day5 where

import AOC.Y2021.Day24 (integer)
import Data.Maybe
import Library

data Almanac = Almanac
  { seeds :: [Integer],
    allMaps :: GardeningMap
  }
  deriving (Show)

newtype GardeningMap = GardeningMap [(String, [Integer])] deriving (Show)

main_pt1 = do
  almanacText <- readFileY 2023 "d5.input"
  let Right Almanac {..} = parse almanac "" almanacText
      GardeningMap m = allMaps
  return $ minimum $ (\s -> foldl (\acc (a, b) -> determineMapValue acc $ isSeedInRange acc <$> chunksOf 3 b) s m) <$> seeds

almanac = Almanac <$> seedsP <*> (GardeningMap <$> many1 maps)

maps = liftM2 (,) mapName numbers

mapName = do name <- many1 (letter <|> char '-'); string " map:"; return name

seedsP = do many1 letter; char ':'; numbers

numbers = concat <$> do many1 integer `sepBy` space

isSeedInRange sd [a, b, c] = if b <= sd && sd <= (b + c - 1) then Just (sd - b + a) else Nothing
isSeedInRange _ _ = Nothing

determineMapValue sd l =
  case catMaybes l of
    [] -> sd
    [s] -> s
