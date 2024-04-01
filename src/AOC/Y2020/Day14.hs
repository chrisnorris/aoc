module AOC.Y2020.Day14 where

import qualified Data.Map as Map
import Library

main =
  sum
    . (snd <$>)
    . Map.toList
    . run Map.empty []
    <$> (lines <$> readFile20 "d14.input")
  where
    intToBits = reverse . toListOf bits
    charIntToBool = toEnum . read . (: [])
    run acc mask = \case
      [] -> acc
      (a : as) -> case matchTest (mkRegex "^mask") a of
        False ->
          let [mem, val] = read <$> getAllTextMatches (a =~ "[0-9]+") :: [Int]
              newVal :: Int =
                0
                  & partsOf (taking 64 bits)
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

    runV2 acc mask = \case
      [] -> acc
      (a : as) -> case matchTest (mkRegex "^mask") a of
        False ->
          let [mem, val] = read <$> getAllTextMatches (a =~ "[0-9]+") :: [Int]
              addressDecoder i = \case
                '0' -> i
                '1' -> True
                'X' -> True -- floating
              newMem :: Int =
                0
                  & partsOf (taking 64 bits)
                  .~ reverse
                    (zipWith addressDecoder (intToBits mem) mask)
           in run (Map.insert newMem val acc) mask as
        True ->
          let msk = a =~ "[X,0-9]+" :: String
           in run acc (replicate 28 'X' <> msk) as
