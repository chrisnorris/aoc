module AOC.Y2022.Day15 where

import AOC.Y2021.Day24 (integer)
import Data.List
import Library
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim

getParsedInput =
  fmap (bimap toSensor (toSensor . drop 2) . break (== ':')) <$> inpStr 2022 "d15.input"
  where
    -- return x

    skipTillEq = manyTill anyChar (char '=')
    fromSensorData = skipTillEq >> integer
    toSensor = parse (liftM2 (,) fromSensorData fromSensorData) mempty

main_pt1 = undefined

main_pt2 = undefined
