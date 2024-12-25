module AOC.Y2022.Day15 where

import           Library

import           Data.List
import           Text.Parsec
import           Text.Parsec.Prim
import           Text.Parsec.Char
import           Text.Parsec.Combinator

import           AOC.Y2021.Day24                ( integer )


getParsedInput =
  fmap (bimap toSensor (toSensor . drop 2) . break (== ':')) <$> inpStr 2022 "d15.input"
  -- return x
  
 where 
       skipTillEq = manyTill anyChar (char '=')
       fromSensorData = skipTillEq >> integer
       toSensor = parse (liftM2 (,) fromSensorData fromSensorData) mempty

main_pt1 = undefined

main_pt2 = undefined
