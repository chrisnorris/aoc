module AOC.Y2022.Day3 where

import Library hiding (fromList, toList)
import Data.Set hiding (splitAt)
import Data.Maybe(fromMaybe)
import qualified Data.Map  as Map

main_pt1 = do
  codes <- inpStr 2022 "d3.input"
  let halfway = midpoint <$> codes
      items =
        uncurry intersection . toSetTuple <$> zipWith splitAt halfway codes
  return $ sum (priority . head . toList <$> items)
  where midpoint = truncate . (/ 2) . toRational . length

main_pt2 = do
  m <- inpStr 2022 "d3.input"
  return $ sum ((priority <$> head . toList) . intersectAll <$> splitEvery 3 m)

priority c = fromMaybe (error "missing") (Map.lookup c priorities)
  where priorities = Map.fromList $ (['a' .. 'z'] <> ['A' .. 'Z']) `zip` [1 ..]

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = as : splitEvery n bs where (as, bs) = splitAt n xs

intersectAll :: Ord a => [[a]] -> Set a
intersectAll l =
  let (a, b) = bimap (fromList . head) (fmap fromList) (splitAt 1 l)
  in  Prelude.foldl intersection a b

toSetTuple = bimap fromList fromList
