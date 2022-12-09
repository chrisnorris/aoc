module AOC.Y2021.Day8 where
import Library
import qualified Data.Set as Set
import qualified Data.Map as Map

import Data.Char(intToDigit)

main = main8ii Full -- 1070957

main8i source =
  sourceWith source drop
    >>= print . sum . getDigits length [2, 3, 4, 7] [] . concat

main8ii source = do
  outputValues    <- sourceWith source drop
  groupSeenDigits <- map (getDigits id [2, 3, 4, 7, 5, 6] []) <$> sourceWith source take
  let toDecodedMap =
        (\[sixes, fives, [eight], [four], [seven], [one]] ->
            let (nine, sixes') =
                  filterBy (Set.fromList four) (Set.fromList <$> sixes)
                (three, fives') =
                  filterBy (Set.fromList one) (Set.fromList <$> fives)
                (zero, six) = filterBy (Set.fromList one) sixes'
                (five, two) = filterBy' (head six) fives'
            in  Map.fromList
                  $     [ unwrapAsList zero
                        , sort one
                        , unwrapAsList two
                        , unwrapAsList three
                        , sort four
                        , unwrapAsList five
                        , unwrapAsList six
                        , sort seven
                        , sort eight
                        , unwrapAsList nine
                        ]
                  `zip` [0 ..]
          )
          <$> groupSeenDigits
  let finalNumbers = zipWith
        ((sequence .) . (<$>) . flip (Map.lookup . sort))
        toDecodedMap
        outputValues

      nums :: [Int] =
        (\case Just digits -> read $ intToDigit <$> digits )
          <$> finalNumbers
  print $ sum nums
 where
  filterBy  = partition . Set.isSubsetOf
  filterBy' = partition . flip Set.isSubsetOf

unwrapAsList = Set.toList . head

getDigits f [] acc b = acc
getDigits f (m : ms) acc b =
  let (a, c) = partition ((m ==) . length) b in getDigits f ms (f a : acc) c

sourceWith source f = concatMap (map words . f 1 . wordsWhen (== '|'))
  <$> inp21Str ("d8.input" <> show source)
