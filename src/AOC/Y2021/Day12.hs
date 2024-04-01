module AOC.Y2021.Day12 where

import Control.Lens
import qualified Data.Map as Map
import Library (Source (..), inp21Str)
import Text.ParserCombinators.Parsec as Parsec

main =
  main11 Sample

-- >>= print
-- >> main11 Full >>= print

main11 :: Source -> IO ()
main11 source = do
  a <- map (parse parseLink "") <$> getInput source
  let adjs =
        foldl
          ( \m (Right (k1, k2)) ->
              let m1 = update k1 k2 m in update k2 k1 m1
          )
          Map.empty
          a

  let paths adjs acc orig =
        let c = (last orig)
         in case Map.lookup c adjs of
              -- build update when we visit a place mark it as so
              Just n -> (\(e, _) -> paths (updtae adjs n c) acc (orig <> [e])) `concatMap` unvisited n
              Nothing -> [orig] <> acc
      unvisited = filter ((== 0) . snd)

      -- need to go over all keys and in those lists
      -- look up those adjs to present coord then for that list amend all
      -- as keys eg 3 -> [4,5] so need to lookup 4, 5 and update the 3 link
      -- to visited.
      --  mp ^. (at e . _Just) & (ix 4 . _1) .~ 1 e.g.
      --  mp ^. (at (Caps "A") . _Just) & (ix 4 . _1) .~ 1 e.g.
      -- (at (Caps "A") . _Just) . (ix 2 . _2) .~ 33333 $ m2

      -- for in n modify adjs

      updtae mp vs k =
        -- at the position in the list of the c ie c -> [A,B], a -> [C -> 1,B]
        -- instead of ix 1 need the position of k
        foldl
          ( \m v ->
              let idx = firstOcc (m ^. at v . _Just) k
               in (at v . _Just) . (ix (idx - 1) . _2) .~ 1 $ m
          )
          mp
          (fst <$> vs)

      firstOcc ls v = head [(snd m) | m <- (fst <$> ls) `zip` [1 ..], (fst m) == v]

  -- print adjs
  -- print $ updtae adjs [(Lower "end",0),(Lower "b",0),(Lower "c",0),(Start "start",0)] (Caps "A")
  print $ paths adjs [] [Start "start"]
  where
    getInput source = inp21Str ("d12.input" <> show source)
    update k1 k2 m =
      case Map.lookup k1 m of
        Nothing -> Map.insert k1 [(k2, 0)] m
        Just v -> Map.insert k1 ((k2, 0) : v) m

data ParseR where
  Caps :: String -> ParseR
  Lower :: String -> ParseR
  Start :: String -> ParseR
  End :: String -> ParseR
  deriving (Eq, Ord, Show)

parseLink :: Parser (ParseR, ParseR)
parseLink = do
  let pathName =
        Start
          <$> Parsec.try (string "start")
            <|> Caps
          <$> Parsec.try (many1 (oneOf ['A' .. 'Z']))
            <|> Lower
          <$> Parsec.try (many1 (oneOf ['a' .. 'z']))
            <|> End
          <$> Parsec.try (string "end")
  s <- pathName
  char '-'
  s2 <- pathName
  return (s, s2)
