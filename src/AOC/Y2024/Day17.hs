module AOC.Y2024.Day17 where

import AOC.Y2021.Day24 (integer)
import Library

main_pt1 = do
  d@Device {..} <- getDevice
  return $ loop (pairs program []) 0 d []

main_pt2 = do
  d <- getDevice
  let p = program d
  let pp = pairs p []

  -- forM_ x (appendFile "tmp-day172025.txt" . (++ "\n") . show)
  -- 281077810000090
  -- 2251077810000090

  -- forM [0..1000] $ \a -> do
  --   let v = 281467099000090 + 10000000000 * a
  --   let d2 = loop pp 0 (d {registerA = v}) []
  --   return (v, d2, length d2)

  -- ///
  return $ loop2 pp p d 281467099000090

loop2 pp p d a =
  let d2 = loop pp 0 (d {registerA = a}) []
   in if (d2 == p) then a else loop2 pp p d (a + 1)

getDevice = do
  m <- readFileY 2024 "d17.input"
  let (Right Device {..}) = parse device "" m
  return Device {..}

combo _ 0 = 0
combo _ 1 = 1
combo _ 2 = 2
combo _ 3 = 3
combo Device {..} 4 = registerA
combo Device {..} 5 = registerB
combo Device {..} 6 = registerC
combo Device {..} 7 = undefined

loop _ 8 d output = reverse output
-- loop _ 3 d output = concat $ show <$> (reverse output)
loop i ip d o =
  case i !! fromIntegral ip of
    (0, op) -> loop i (ip + 1) (d {registerA = d ~/ op}) o
    (1, op) ->
      loop i (ip + 1) (d {registerB = registerB d `xor` op}) o
    (2, op) -> loop i (ip + 1) (d {registerB = combo d op `mod` 8}) o
    (3, op) ->
      if registerA d == 0
        then loop i (ip + 1) d o
        else loop i op d o
    (4, op) ->
      let bxc = registerB d `xor` registerC d
       in loop i (ip + 1) (d {registerB = bxc}) o
    (5, op) ->
      let c = combo d op `mod` 8
       in loop i (ip + 1) d (c : o)
    (6, op) -> loop i (ip + 1) (d {registerB = d ~/ op}) o
    (7, op) -> loop i (ip + 1) (d {registerC = d ~/ op}) o

(~/) d@Device {..} op =
  floor $ fromIntegral registerA / (2 ^ combo d op)

pairs [] acc = acc
pairs (x : y : xs) acc =
  pairs xs (acc <> [(x, y)])

data Device = Device
  { registerA :: Integer,
    registerB :: Integer,
    registerC :: Integer,
    program :: [Integer]
  }
  deriving (Show)

device =
  Device
    <$> register 'A'
    <*> register 'B'
    <*> register 'C'
    <*> programP

register regName = do
  string "Register"
  spaces
  c <- char regName
  char ':'
  integer

programP =
  do
    string "Program:"
    spaces
    concat <$> many integer `sepBy` char ','
