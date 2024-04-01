module AOC.Y2020.Day4 where

import qualified Data.Map as Map
import Library

main =
  readFile20 "d4.input"
    >>= (return . length . filter (== True))
      . map (getIds . words)
      . lines
      . parseFile
      . lines

getIds =
  (sort expected ==)
    . sort
    . filter (/= "cid")
    . Map.keys
    . Map.fromList
    . map (break (== ':'))
  where
    expected = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
