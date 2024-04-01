module AOC.Y2020.Day23 where

import Library

main :: IO ()
main = (day23 100) >>= print

day23 n = do
  let samp = asCircularList "389125467"
  let final = asCircularList "198753462"
  print $ repeatN n simulate final
  where
    simulate inp@(focus -> Just fs) = rotate next3 drop3
      where
        rotate n3 (rotateTo (adj (fs - 1) (snd n3)) -> Just refocussed) =
          fromJust $ rotateTo (fst n3) $ foldr insertL refocussed (snd n3)
        rotate n3 _ = error "missing element for rotation"
        adj focus chosen = case focus `compare` 0 of
          EQ -> adj 9 chosen
          _ -> rejig chosen
            where
              rejig chosen@((focus `elem`) -> True) = adj (focus - 1) chosen
              rejig _ = focus
        next3 = fromJust . uncons . reverse . take 4 . rightElements . rotR $ inp
        drop3 = repeatN 3 removeR . rotR $ inp
    simulate _ = error "cant be empty circular list"
