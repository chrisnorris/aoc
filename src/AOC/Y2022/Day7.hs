module AOC.Y2022.Day7 where

import AOC.Y2021.Day24 (integer)
import Data.List (isPrefixOf, transpose)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Library
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim

data Terminal where
  Cd :: String -> Terminal
  Ls :: Terminal
  Dir :: String -> Terminal
  FileData :: Integer -> String -> Terminal
  deriving (Show)

main_pt1 = do
  x <- inpStr 2022 "d7.input"
  let parsedTerminal = sequence (parse commands "terminal" <$> x) ^. _Right
      collapseFiles = foldr (uncurry (M.insertWith (<>))) M.empty $ init $ eval parsedTerminal
      sumFilesByDir = (\t -> (sum [x | FileData x _ <- t])) `M.map` collapseFiles
      sumOverDirs dir = M.foldrWithKey (\ky a acc -> if dir `isPrefixOf` ky then (acc + a) else acc) 0 sumFilesByDir
  -- 1778099
  return (sum $ filter (<= 100000) $ sumOverDirs <$> M.keys collapseFiles)
  where
    commands = try cd <|> try ls <|> try directory <|> try file

-- main_pt2 =

eval :: [Terminal] -> [(String, [Terminal])]
eval = foldl go [("", [])]
  where
    go a@((d, f) : b) = \case
      Cd "/" -> ("./", []) : a
      Cd ".." ->
        ((intercalate "/" $ (init . init) (splitOn "/" d)) <> "/", []) : a
      Cd dirName -> (d <> dirName <> "/", []) : a
      FileData i s -> (d, [FileData i s]) : a
      _ -> a

ls = do
  command
  string "ls"
  return Ls

cd =
  Cd
    <$> do
      command
      string "cd"
      space
      string ".." <|> string "/" <|> many letter

directory =
  Dir <$> do
    string "dir"
    space
    many letter

file =
  FileData
    <$> integer
    <*> many (choice [letter, char '.'])

command = char '$' *> space
