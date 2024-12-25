module AOC.Y2022.Day20 where

import Control.Lens
import Data.CircularList as CL
import Data.List as L
import Library (inpStr)

main_pt1 = do
  x :: [Int] <- map read <$> inpStr 2022 "d20.input"

  return $ p2sns (x `zip` [0 ..])

main_pt2 = undefined

aa l 7 = sortBy (\(a, b) (c, d) -> b `compare` d) l
aa l n =
  let (e, f) = l !! n
      zz = length l
      shft = if e >= 0 then mod (f + e) (zz - 1) else mod (f + e - 1) zz
      newe = (e, shft + 5000)
      newl = l & ix n .~ newe
      final =
        ( \z@(a, b) ->
            case (f + e) > (zz - 1) of
              False -> if b > f && b <= shft then subtract 1 <$> z else z
              True -> if b >= shft && b < f then (+ 1) <$> z else z
        )
          <$> newl
      ffinal = final & ix n .~ (e, shft)
   in aa ffinal (n + 1)

p2sns s =
  let e = length s
      s2 = aa s 0
   in case find (\(a, b) -> a == 0) s2 of
        Just (a, b) ->
          sumOf
            (folded . _1)
            ((s2 !!) . flip mod e . (+) b <$> [1000, 2000, 3000])
        Nothing -> error "missing 0"
