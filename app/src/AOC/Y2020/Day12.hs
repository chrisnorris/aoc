module AOC.Y2020.Day12 where

import Library

main = do
  dirs :: [(String, Int)] <-
    map (\x -> (take 1 x, read $ drop 1 x)) . lines <$> readFile20 "d12.input"
  foldM moveShip (0, 0, 0) dirs

moveShip current@(x, y, s) ins =
  print ("Ins:" <> show ins <> show current) >> case ins of
    ("N", v) ->
      let r = (x, y + v, s)
       in do
            print r
            return r
    ("S", v) ->
      let r = (x, y - v, s)
       in do
            print r
            return r
    ("E", v) ->
      let r = (x + v, y, s)
       in do
            print r
            return r
    ("W", v) ->
      let r = (x - v, y, s)
       in do
            print r
            return r
    ("L", v) ->
      let r = (x, y, (s + v) `mod` 360)
       in do
            print r
            return r
    ("R", v) ->
      let r =
            ( x,
              y,
              if (s - v) < 0 then (s - v + 360) `mod` 360 else (s - v) `mod` 360
            )
       in do
            print r
            return r
    ("F", v) -> case s of
      0 ->
        let r = (x + v, y, s)
         in do
              print r
              return r -- east
      90 ->
        let r = (x, y + v, s)
         in do
              print r
              return r -- north
      180 ->
        let r = (x - v, y, s)
         in do
              print r
              return r -- west
      270 ->
        let r = (x, y - v, s)
         in do
              print r
              return r -- south
