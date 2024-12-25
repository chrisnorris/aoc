module AOC.Y2023.Day18 (DigPlan (..), main_pt1, main_pt2) where

import AOC.Y2021.Day24 (integer)
import qualified Data.Array as A
import Data.Char
import Data.List
import qualified Data.Map as M
import Library

data DigPlan = DigPlan
  { direction :: Char,
    steps :: Int,
    color :: String
  }
  deriving (Show)

main_pt1 = sum . (width' <$>) . ar2map <$> getDigplans

main_pt2 =
  getDigplans >>= printAr

-- do
--    al <- lines <$> readFileY 2023 "d18.input"
--    let (Right plans) = sequence $ parse digPlanParser "" <$> al
--    return plans

getDigplans = do
  al <- lines <$> readFileY 2023 "d18.input.sam"
  let (Right plans) = sequence $ parse digPlanParser "" <$> al
  return $
    A.listArray ((-500, -500), (900, 900)) (repeat 0)
      A.// ((,1) <$> snd (maxMin plans))

digPlanParser =
  DigPlan
    <$> upper
    <*> (fromIntegral <$> integer)
    <*> (concat <$> rgbHex)

rgbHex =
  between
    (char '(')
    (char ')')
    (char '#' >> many1 (lower <|> digit) `sepBy` char ',')

planToIndexAssoc DigPlan {..} = direction

maxMin = foldl myf ((0, 0), [])

width [] = 0
width "#" = 1
width line =
  if all (== ' ') line
    then 0
    else
      let fandL = f $ reverse $ f (imap (,) line)
          ((s, _), (e, _)) = (last fandL, head fandL)
       in 1 + e - s
  where
    f = dropWhile ((/= '#') . snd)

switch ((0, ' ') : a) '#' = (1, '#') : a
switch ((0, ' ') : a) ' ' = (0, ' ') : a
switch ((1, '#') : a) ' ' = (1, ' ') : a
switch ((1, '#') : a) '#' = (1, '#') : a
switch ((1, ' ') : a) _ = (1, '#') : a
switch ((0, '#') : a) _ = (1, '#') : a

width2 = foldl switch [(0, ' ')] "  #   #  # #  "

switch' (False, n, acc, ' ') '#' = (True, n + 1, [1] <> acc, '#')
switch' (False, n, acc, _) ' ' = (False, n, acc, ' ')
switch' (True, n, acc, '#') '#' = (True, n + 1, [1] <> acc, '#')
-- switch' (True, n, acc, '#') ' ' = (False, 0, [n+1] <> acc, ' ')
switch' (True, n, acc, '#') ' ' = (False, 0, [n + 1] <> acc, ' ')

width' l = let (_, _, a, _) = foldl switch' (False, 0, [], head l) l in sum a

asDirectionSteps DigPlan {..} = (direction, steps)

myf (endPt@(y, x), acc) =
  let p =
        ( \case
            ('R', st) -> [(y, x + s) | s <- [0 .. st]]
            ('L', st) -> [(y, x - s) | s <- [0 .. st]]
            ('U', st) -> [(y + s, x) | s <- [0 .. st]]
            ('D', st) -> [(y - s, x) | s <- [0 .. st]]
        )
   in (\v -> (last v, acc ++ v)) . p . asDirectionSteps

printAr a =
  let ((lx, ly), (ux, uy)) = A.bounds a
      xx = chunksOf (ux - lx + 1) (A.elems $ (\x -> if x == 0 then ' ' else '#') <$> a)
   in forM_ xx print

ar2map a =
  let ((lx, ly), (ux, uy)) = A.bounds a
   in chunksOf (ux - lx + 1) (A.elems $ (\x -> if x == 0 then ' ' else '#') <$> a)
