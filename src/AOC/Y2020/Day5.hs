module AOC.Y2020.Day5 where

import Library
import qualified Data.Map as Map

main4b = do
  valids <-
    map snd . filter ((True ==) . fst) . map (getId4b . words) . lines . parseFile . lines
      <$> readFile20 "d4.input"
  return (valids, map validateMap valids)

getId4b inputList =
  let passportFields = Map.fromList $ for inputList (break (== ':'))
   in ( (sort expected ==) . sort $
          filter (/= "cid") $
            Map.keys
              passportFields,
        passportFields
      )
  where expected = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

validateMap :: Map.Map String String -> [Bool]
validateMap = Map.foldrWithKey validatePassport []
  where
    validatePassport "byr" v acc = validateYear 1920 2002 v : acc
    validatePassport "iyr" v acc = validateYear 2010 2020 v : acc
    validatePassport "eyr" v acc = validateYear 2020 2030 v : acc
    validatePassport "hgt" v acc | matchTest hgtIn v = validateH 150 193 v : acc
    validatePassport "hgt" v acc | matchTest hgtCm v = validateH 59 76 v : acc
    validatePassport "hcl" v acc = matchTest hairC v : acc
    -- validatePassport "ecl" v acc = acc
    -- validatePassport "pid" v acc = acc

    validatePassport _ v acc = acc

validateYear st end y
  | matchTest numbs (trimmed y) =
    let psd = read y :: Int in (psd >= st && psd <= end)
validateYear _ _ _ = False

numbs = mkRegex "^:[0-9]{4}$"

hgtIn = mkRegex "^[0-9]+in$"

hgtCm = mkRegex "^[0-9]+cm$"

hairC = mkRegex "#[0-9,[a-f]{6}"

validateH st end y =
  let psd = read (trimEnd y) :: Int in (psd >= st && psd <= end)