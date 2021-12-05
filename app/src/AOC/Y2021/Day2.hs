module AOC.Y2021.Day2 where

import Library

main2 = readInstr >>= navigate >>= debug
  
 where navigate = return . 
                     foldr (\[ins, stp] (depth , hPos) -> 
                        case ins of "forward" -> (depth, read stp + hPos)
                                    "up"      -> (depth - read stp , hPos)
                                    "down"    -> (depth + read stp , hPos)) (0,0)
       readInstr = map words <$> inp21Str "d2.input"

main2ii = readInstr >>= navigate >>= print

 where navigate = return . 
                     foldr (\[i, stp] (l, ins@(depth , hPos, aim)) -> 
                        case i of   "forward" -> (l<>[ins], (depth + aim * read stp, hPos + read stp, aim))
                                    "up"      -> (l<>[ins], (depth , hPos, aim - read stp))
                                    "down"    -> (l<>[ins], (depth , hPos, aim + read stp))) ([], (0,0,0))
       readInstr = reverse . map words <$> inp21Str "d2.input"
