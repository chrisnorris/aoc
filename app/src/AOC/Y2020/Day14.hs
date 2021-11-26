module AOC.Y2020.Day14 where

import Library
import qualified Data.Map as Map

main =
  sum
    . (snd <$>)
    . Map.toList
    . run Map.empty []
    <$> (lines <$> readFile' "d14.input")
  where
    intToBits = reverse . toListOf bits
    charIntToBool = toEnum . read . (: [])
    run acc mask ll = case ll of
      [] -> acc
      (a : as) -> case matchTest (mkRegex "^mask") a of
        False ->
          let [mem, val] = read <$> getAllTextMatches (a =~ "[0-9]+") :: [Int]
              newVal :: Int =
                0 & partsOf (taking 64 bits)
                  .~ reverse
                    ( zipWith
                        (\m i -> if m == 'X' then i else charIntToBool m)
                        mask
                        (intToBits val)
                    )
           in run (Map.insert mem newVal acc) mask as
        True ->
          let msk = a =~ "[X,0-9]+" :: String
           in run acc (replicate 28 'X' <> msk) as

    runV2 acc mask ll = case ll of
      [] -> acc
      (a : as) -> case matchTest (mkRegex "^mask") a of
        False ->
          let [mem, val] = read <$> getAllTextMatches (a =~ "[0-9]+") :: [Int]
              addressDecoder m i = case m of
                '0' -> i
                '1' -> True
                'X' -> True -- floating
              newMem :: Int =
                0 & partsOf (taking 64 bits)
                  .~ reverse
                    (zipWith addressDecoder mask (intToBits mem))
           in run (Map.insert newMem val acc) mask as
        True ->
          let msk = a =~ "[X,0-9]+" :: String
           in run acc (replicate 28 'X' <> msk) as
