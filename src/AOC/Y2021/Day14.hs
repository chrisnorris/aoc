{-# LANGUAGE OverloadedStrings #-}

module AOC.Y2021.Day14 where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Library

main = main14 Sample >>= print >> main14 Full >>= print

main14 source = do
  polymerT <- T.pack . head <$> getInput source
  piRules <-
    map ((\[a, b, c] -> (T.pack a, T.pack c)) . words)
      . drop 2
      <$> getInput source
  --
  let m =
        Map.fromList $
          (\(e1, e2) -> (e1, T.take 1 e1 <> e2 <> T.drop 1 e1))
            <$> piRules

  let finalPolymer = foldl' (\p i -> aux 0 m p T.empty) polymerT [1 .. 10]
      distinctPolymerElements = Set.fromList $ T.unpack finalPolymer

      results =
        (\t -> (t, T.count (T.pack [t]) finalPolymer))
          <$> ((Set.toList distinctPolymerElements))

  return $ let counts = snd <$> results in (maximum counts, minimum counts)
  where
    getInput source = inp21Str ("d14.input" <> show source)

aux n m "" acc = acc
aux n m poly acc =
  let (a : as) = T.unpack poly
   in if null as
        then acc
        else case Map.lookup (T.pack [a, head as]) m of
          Just f ->
            let x = (if n == 0 then acc <> f else acc <> T.drop 1 f)
             in aux (n + 1) m (T.pack as) x
