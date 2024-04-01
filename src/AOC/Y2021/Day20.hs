module AOC.Y2021.Day20 where

import qualified Data.Array as A
import Data.Char (digitToInt)
import Library
  ( Source (..),
    chunksOf,
    forM,
    inp21Str,
    liftM2,
  )

main = main20 Full >>= print -- >> main10ii Full >>= print

main20 source = do
  ((xdc, ydc), inputCode) <- generateSourceArrWith (take 1)
  ((xd, yd), inputArray) <- generateSourceArrWith (drop 2)

  let valid (x, y) = x >= 0 && y >= 0 && x < xd && y < yd
      adjacents (x, y) arr v =
        [ if valid coords then arr A.! coords else v
          | m <- [-1, 0, 1],
            n <- [-1, 0, 1],
            let coords = (x + m, y + n)
        ]

  let update x arr =
        let (l, h) = A.bounds arr
         in A.array
              (l, h)
              [ (ix, inputCode A.! (0, indexValue $ adjacents ix arr x))
                | (ix, e) <- A.assocs arr
              ]

  -- need to paint to depth 2 around updated array
  -- first layer is computed second layer is known initially to be all
  -- zeros but this evolves:
  -- let new = A.array ((0,0), (xd - 1+2, yd - 1+2)) $ (grow . update) inputArray

  let boundArr v arr =
        let (z, (x, y)) = A.bounds arr
         in A.array (z, (x + 2, y + 2)) $
              (\i -> [((i, 0), v), ((i, y + 2), v)])
                `concatMap` [0 .. x + 2]
                <> (\j -> [((0, j), v), ((y + 2, j), v)])
                  `concatMap` [1 .. y + 1]
                <> [((x + 1, y + 1), e) | ((x, y), e) <- A.assocs arr]

  let zeroBoundArr1 = boundArr 1 $ update 0 $ boundArr 0 inputArray
  let zeroBoundArr2 = boundArr 0 $ update 1 $ boundArr 1 zeroBoundArr1
  -- let zeroBoundArr3 = boundArr 1 $ update 0 $ boundArr 0 zeroBoundArr2

  forM (pretty zeroBoundArr1 (yd + 4)) putStrLn
  print $ A.bounds zeroBoundArr1
  forM (pretty zeroBoundArr2 (yd + 8)) putStrLn
  print $ A.bounds zeroBoundArr2

  -- let new = A.array ((0,0), (xd - 1, yd - 1)) $ update inputArray
  -- let new2 = A.array ((0,0), (xd - 1, yd - 1)) $ update new

  -- print (xd,yd)
  -- print (xdc,ydc)
  -- forM (pretty inputArray yd) putStrLn
  -- putStrLn "---"
  -- forM (pretty new yd) putStrLn
  -- putStrLn "---"
  -- forM (pretty new2 yd) putStrLn
  -- print $ length inputCode
  return 0 -- (y, inputCode A.! (0,y))
  where
    pretty arr yd =
      chunksOf yd [if e == 1 then '#' else '.' | ((x, y), e) <- A.assocs arr]
    generateSourceArrWith f = do
      c@(xdc, ydc) <- liftM2 (,) length (length . head) . f <$> getInput source
      a <-
        A.listArray ((0, 0), (xdc - 1, ydc - 1))
          <$> (map toInt . concat . f <$> getInput source)
      return (c, a)

getInput source = inp21Str ("d20.input" <> show source)

-- gen with reverse
-- take 10 $ 0^..unfolded (\b -> if b == 9 then Nothing else Just (2^b, b+1))
pows = [256, 128, 64, 32, 16, 8, 4, 2, 1]

indexValue code =
  if length code < 9 then error "!!!" else sum $ zipWith (*) pows code

class AsIntVal a where
  toInt :: a -> Int

instance AsIntVal Char where
  toInt '#' = 1
  toInt _ = 0
