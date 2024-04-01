module AOC.Y2020.Day15 where

import qualified Data.Map as Map
import Library

main :: IO ()
main = void (day15 [6, 19, 0, 5, 7, 13, 1])

day15 :: [Integer] -> IO (Integer, Integer)
day15 inp =
  return $
    go (Map.fromList $ zipWith (\i e -> (i, [e])) inp [1 ..]) 1 8
  where
    go m spoken loop =
      if loop > 30000000
        then (spoken, loop)
        else case Map.lookup spoken m of
          Just (x : y : xx) ->
            let prevDiff = x - y
             in case Map.lookup prevDiff m of
                  Nothing -> go (Map.insert prevDiff [loop] m) prevDiff (loop + 1)
                  Just x ->
                    go (Map.insert prevDiff (loop : x) m) prevDiff (loop + 1)
          Just [x] ->
            let prev = fromJust (error "nope") (Map.lookup 0 m)
             in go (Map.insert 0 (loop : prev) m) 0 (loop + 1)
          Nothing -> case Map.lookup 0 m of
            Nothing -> go (Map.insert 0 [loop] m) 0 (loop + 1)
            Just prev -> go (Map.insert 0 (loop : prev) m) 0 (loop + 1)
