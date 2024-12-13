module AOC.Y2024.Day11 where

import qualified Data.Set as S
import Library hiding (deep)
import Control.Monad.Par           hiding (parMap)
import Control.Parallel.Strategies hiding (parMap)

main_pt1 = do
  m <- words <$> readFileY 2024 "d11.input"
  let n = chunksOf 2 (blink 10 m)
  -- return $ length <$> (blink 25 <$> n)
  -- l <- runEvalIO ((length . (blink 15)) `parMap` n)
  -- return $ sum l
  return $ blinkChunk n []

main_pt2 = do
  m <- words <$> readFileY 2024 "d11.input"
  let n = chunksOf 4 (blink 30 m)
  l <- runEvalIO ((length . blink 45) `parMap` n)
  return $ sum l


parMap ::  (t -> a) -> [t] -> Eval [a]
parMap f [] = return []
parMap f (a:as) = do
    b  <- rpar(f a)
    bs <- parMap f as
    return (b:bs)

blinkChunk as acc
    = foldl (\ acc a -> acc ++ [length (blink 15 a)]) acc as

blink a n
  | a == 0 = n
  | otherwise = blink (a-1) (evolve n [])

 
evolve [] acc =  acc
evolve l@(a:as) acc
  | a == "0" = evolve as ("1":acc)
  | evenDigits a =
     let (lh,rh) = lr a in evolve as (rh : lh : acc)
  | otherwise = evolve as (mul2024 a : acc)

mul2024 :: String -> String
mul2024 s = show $  2024 * read s

evenDigits s =
  length s `mod` 2 == 0

lr s = 
  let l = round $ fromIntegral (length s ) / 2
      rh :: Int = read $ drop l s
  in (take l s, show rh) 