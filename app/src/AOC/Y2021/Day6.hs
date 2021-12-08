module AOC.Y2021.Day6 where

import qualified Data.Vector as DV
import Control.Monad (forM)
import System.FilePath.Posix

-- 80 is 388419
main = do
  fishesL :: [Int] <- read . (<> "]") . ("[" <>) <$> readFile21 "d6.input"

  forM (DV.fromList <$> buckets [] fishesL) $ 
      print . length . sim 80

 where sim 0 fishes = fishes
       sim n fishes = sim (n-1) (evolve fishes)

       evolve = DV.foldl (\b fish  -> b DV.++ newfish fish) DV.empty
       newfish 0 = DV.fromList [6, 8]
       newfish x = DV.fromList [x - 1]

main6ii = do
  fishesL :: [Int] <- read . (<> "]") . ("[" <>) <$> readFile21 "d6.input"
  print $ length . sim 256 <$> (DV.fromList <$> buckets [] fishesL)

 where sim 0 fishes = fishes
       sim n fishes = sim (n-1) (evolve fishes)

evolve l = 
  let d = DV.map (subtract 1) l
      (e,f) = DV.partition  (== (-1)) d
      g = DV.length e in (f DV.++ DV.replicate g 6) DV.++ DV.replicate g 8

buckets a [] = a 
buckets a x = buckets (take 30 x:a) (drop 30 x)

readFile21 = readFile . ("app/input/2021" </>)
