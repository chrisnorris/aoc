module AOC.Y2024.Day18 where

import AOC.Y2021.Day24 (integer)
import Library

main_pt1 = do
  m <- readFileY 2024 "d18.input"
  
  let (Right bytePos) = sequence $ (parse tp "") <$> (lines m)
  -- let x = lines m

  -- let q = getNextMovesWithAcc (0,0) [] bytePos

  l <- listAll (0,0) [] [] (take 1024 bytePos)
  return $ minimum $ length <$> (concat l)

main_pt2 = undefined

tp = do
  x <- integer
  char ','
  y <- integer
  return (x,y)

cursor l c = [(x, y) | let w = length l - 1, x <- [0 .. w], y <- [0 .. w], (l !! x) !! y == c]

getNextMovesWithAcc p@(x,y) acc bp =
  let c = [(nx, ny) | dy <- [-1, 0, 1], dx <- [-1, 0, 1], abs dx /= abs dy, let nx = x+dx, let ny = y+dy,  nx>=0 && nx<=70, ny>=0 && ny<=70,
            (nx, ny) `notElem` bp]
      a = filter (`notElem` acc) c
  in (a, a <> acc)

listAll pt acc ac bp = do
  let (npts, newAcc) = getNextMovesWithAcc pt acc bp
  p <- if null npts then return [[]] else
       forM npts $ \ p ->
        if p == (70, 70) then return [[ac]] else listAll p newAcc (p:ac) bp
  return (concat p)
