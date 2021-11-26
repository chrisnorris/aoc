module AOC.Y2020.Day8 where

import Library

main8 =
  readFile' "d8.input" >>= interpret 0 0 . map ((,1) . parseOperand) . lines

main8b = do
  baseCodes <- map ((,1) . parseOperand) . lines <$> readFile' "d8.input"
  let allCodes = zip [0 ..] baseCodes
  forM
    ( foldr
        ( \(i, ins) acc -> case ins of
            (Jmp n, 1) ->
              (take i baseCodes ++ [(Nop n, 1)] ++ drop (i + 1) baseCodes) : acc
            (Nop n, 1) ->
              (take i baseCodes ++ [(Jmp n, 1)] ++ drop (i + 1) baseCodes) : acc
            _ -> acc
        )
        []
        allCodes
    )
    $ interpretb 0 0

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
