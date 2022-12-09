module AOC.Y2020.Day3 where

import Library

main :: IO ()
main = void day3

day3 = do
  i <- lines <$> readFile20 "d3.input"
  let steps (r, d) = (0, 0) ^.. unfolded
        (\(x, y) ->
          if y < length i then _Just # ((x, y), (x + r, y + d)) else Nothing
        )

      slope (x, y) st = danger (x `divMod` length (head i))
       where
        danger (\(b, p) -> repeat i ^? ix b . ix y . ix p -> Just n) = n : st

  return
    . product
    $ [ length [ x | x <- foldr slope [] (steps slopeC), x == '#' ]
      | slopeC <- ((, 1) <$> [1, 3, 5, 7]) <> [(1, 2)]

      ]
