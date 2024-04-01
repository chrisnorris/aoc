module AOC.Y2020.Day18 where

import Library

main :: IO ()
main = day18 >>= print

day18 = do
  source <- lines <$> readFile20 "d18.input"
  return $ tokenize [] . words <$> source
  where
    tokenize acc = \case
      [] -> acc
      ("+" : as) -> tokenize (Plus : acc) as
      ("*" : as) -> tokenize (Mult : acc) as
      ("(" : as) -> tokenize (OpenParen : acc) as
      (")" : as) -> tokenize (CloseParen : acc) as
      (a : as) -> tokenize (Num (read a) : acc) as

    parse acc = \case
      [] -> acc
      _ -> acc

data Tokens = Plus | Mult | Num Int | OpenParen | CloseParen deriving (Show)
