module AOC.Y2024.Day4 where

import Library
import Text.Regex.TDFA

main_pt1 = do
  input' <- lines <$> parseLists
  return $
    (getAllTextMatchesFwdBck <$>)
      `concatMap` [ input',
                    transpose input' id id,
                    diagonals input' `concatMap` [1, 0],
                    drop 1 $ diagonals (transpose input' id id) 1,
                    drop 1 $ diagonals (transpose input' reverse reverse) 0
                  ]

main_pt2 = do
  i <- lines <$> parseLists
  let l = length i
      valid (l1, l2) = sort l1 == "MS" && sort l2 == "MS"
  return
    [ (x, y)
    | x <- [1 .. l - 2],
      y <- [1 .. l - 2],
      let p x y (dx, dy) = (i !! (x + dx)) !! (y + dy),
      p x y (0, 0) == 'A',
      valid (p x y <$> [(-1, -1), (1, 1)], p x y <$> [(-1, 1), (1, -1)])
    ]

parseLists = readFileY 2024 "d4.input"

getAllTextMatchesFwdBck s =
  length $ (\x -> getAllTextMatches $ x =~ "XMAS" :: [String]) `concatMap` [s, reverse s]

transpose l f g =
  let w = length $ head l in [f ((!! i) <$> l) | i <- g [0 .. w - 1]]

diagonals l f =
  let w = length $ head l
   in [uncurry (!!) <$> l `zip` b f n w | n <- [0 .. w - 1]]
  where
    b f n w
      | f == 1 = [n .. w - 1]
      | otherwise = reverse [0 .. w - 1 - n]
