module AOC.Y2020.Day9 where

import Library

main :: IO ()
main = day9 >>= print

day9 = do
  baseCodes :: [Int] <- map read . lines <$> readFile20 "d9.input"
  let preamble = take 25 baseCodes
  let input = drop 25 baseCodes
  getValids preamble input


interpretb pc acc instructions =
  if pc == length instructions
    then return (acc, "TERMS")
    else case (instructions !! pc, pc) of
      (x@(Jmp n, 1), _) ->
        print (pc, acc, x)
          >> interpret
            (pc + n)
            acc
            (take pc instructions ++ [(Jmp n, 2)] ++ drop (pc + 1) instructions)
      (x@(Jmp n, 2), _) -> return (acc, "LOOP")
      (x@(Acc n, 1), _) ->
        print (pc, acc, x)
          >> interpret
            (pc + 1)
            (acc + n)
            (take pc instructions ++ [(Acc n, 2)] ++ drop (pc + 1) instructions)
      (x@(Acc n, 2), _) -> return (acc, "LOOP")
      (x@(Nop n, 1), _) ->
        print (pc, acc, x)
          >> interpret
            (pc + 1)
            acc
            (take pc instructions ++ [(Nop n, 2)] ++ drop (pc + 1) instructions)
      ((Nop n, 2), _) -> return (acc, "LOOP")


interpret pc acc instructions =
  if pc == length instructions
    then return (acc, "TERMS")
    else case (instructions !! pc, pc) of
      (x@(Jmp n, 1), _) ->
        print (pc, acc, x)
          >> interpret
            (pc + n)
            acc
            (take pc instructions ++ [(Jmp n, 2)] ++ drop (pc + 1) instructions)
      (x@(Jmp n, 2), _) -> return (acc, "LOOP")
      (x@(Acc n, 1), _) ->
        print (pc, acc, x)
          >> interpret
            (pc + 1)
            (acc + n)
            (take pc instructions ++ [(Acc n, 2)] ++ drop (pc + 1) instructions)
      (x@(Acc n, 2), _) -> return (acc, "LOOP")
      (x@(Nop n, 1), _) ->
        print (pc, acc, x)
          >> interpret
            (pc + 1)
            acc
            (take pc instructions ++ [(Nop n, 2)] ++ drop (pc + 1) instructions)
      ((Nop n, 2), _) -> return (acc, "LOOP")

getValids preamble input@(a : as) =
  if a `elem` allSubSums preamble []
    then getValids (drop 1 preamble <> [a]) as
    else return a

allSubSums inp acc = case inp of
  [] -> concat $ foldl (\st (a, b) -> ((+ a) <$> b) : st) [] acc
  _ -> allSubSums (drop 1 inp) $ (head inp, drop 1 inp) : acc