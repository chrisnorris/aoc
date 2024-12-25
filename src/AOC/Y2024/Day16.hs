module AOC.Y2024.Day16 where

import AOC.Y2021.Day24 (integer)
import Library

main_pt1 = do
  m <- readFileY 2024 "d16.input.sam"
  let x = lines m
      s = head $ cursor x 'S'
      q = getNextMovesWithAcc x s []

  l <- listAll x s [] []
  return $ concat l -- TODO compute turns and steps in fwd direction for scores , scores <$> l

-- take 6 $ bfsLayers (iterateM (\i -> getNextMoves x i) [s] :: ListT [] (Int,Int))

main_pt2 = undefined

-- scores p:ps = undefined

cursor l c = [(x, y) | let w = length l - 1, x <- [0 .. w], y <- [0 .. w], (l !! x) !! y == c]

-- getNextMoves l p@(x,y) =
--   let c = [(nx, ny) | dy <- [-1, 0, 1], dx <- [-1, 0, 1], abs dx /= abs dy, let nx = x+dx, let ny = y+dy,
--             (l !! nx) !! ny == '.']
--   in c

getNextMovesWithAcc l p@(x, y) acc =
  let c =
        [ (nx, ny)
          | dy <- [-1, 0, 1],
            dx <- [-1, 0, 1],
            abs dx /= abs dy,
            let nx = x + dx,
            let ny = y + dy,
            (l !! nx) !! ny `elem` ['.', 'E']
        ]
      a = filter (`notElem` acc) c
   in (a, a <> acc)

-- loopEm x [] acc ans = ans
-- loopEm x (i:is) acc ans =
--   let (a, newAcc) = getNextMovesWithAcc x i acc in loopEm x (take 1 a) newAcc ((take 1 a) : ans)

-- stuff = do
--   m <- readFileY 2024 "d16.input.sam"
--   let x = lines m
--       s = (head $ cursor x 'S')
--   return (x, s)

listAll x pt acc ac = do
  let (npts, newAcc) = getNextMovesWithAcc x pt acc
  p <-
    if null npts
      then return [[]]
      else forM npts $ \p ->
        if (x !! fst p) !! snd p == 'E' then return [[ac]] else listAll x p newAcc (p : ac)
  return (concat p)
