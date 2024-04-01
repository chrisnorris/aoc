module AOC.Y2022.Day4
  ( main_pt1,
    main_pt2,
  )
where

import qualified Data.Set as S
import Library hiding (fromList, parse)

data Range a = R a a a a
  deriving (Show, Functor)

main_pt1 = do
  x <- inpStr 2022 "d4.input"
  let n :: [Range Int] = toRanges <$> fmap parse x
  return $ sum (valid <$> n)
  where
    valid (R a b c d) = fromEnum ((a <= c) && (b <= d) || (a >= c) && (b >= d))

main_pt2 :: IO Int
main_pt2 = do
  x <- inpStr 2022 "d4.input"
  let n :: [Range Int] = toRanges <$> fmap parse x
  return $ sum $ overlap <$> n
  where
    overlap (R a b c d) =
      (fromEnum . (> 0) . S.size) $
        S.intersection (S.fromList [a .. abs b]) (S.fromList [c .. abs d])

toRanges ((a, b), (c, d)) = read <$> R a b c d

parse = bimap breakHyphens (breakHyphens . drop 1) . breakComma

breakHyphens = break (== '-')

breakComma = break (== ',')
