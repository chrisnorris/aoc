module AOC.Y2021.Day10 where

import Library
  ( Source (..),
    Stack (..),
    inp21Str,
    pop,
    push,
    sempty,
    sort,
  )

main = main10i Full >>= print >> main10ii Full >>= print

main10i :: Source -> IO Int
main10i source =
  (fmap pointsValue <$>)
    . filter unmatch
    . (fst <$>)
    <$> generateInput source
    >>= return . foldl (\acc (Unmatched e) -> acc + e) 0

main10ii :: Source -> IO Integer
main10ii source = do
  res <- generateInput source

  let incompletions = fmap flip' . snd <$> filter (isDone . fst) res
  let results = sort $ sumUp 0 <$> incompletions
  return (results !! max 0 ((length results - 1) `div` 2))
  where
    sumUp :: Integer -> Stack Char -> Integer
    sumUp acc (Stack []) = acc
    sumUp acc (Stack (c : cs)) = sumUp (5 * acc + pointsValue c) (Stack cs)

generateInput :: (Show a) => a -> IO [(Result Char, Stack Char)]
generateInput source =
  (checkSyntax sempty <$>) <$> inp21Str ("d10.input" <> show source)

checkSyntax :: Stack Char -> [Char] -> (Result Char, Stack Char)
checkSyntax acc [] = (Done, acc)
checkSyntax acc (c : cs) =
  if opener c
    then checkSyntax (push c acc) cs
    else case pop acc of
      (Just x, st) ->
        if flip' x == c then checkSyntax st cs else (Unmatched c, acc)
      (Nothing, _) -> (Error, acc)

data Result c = Done | Error | Unmatched c deriving (Show, Functor)

class HasNoMatch r where
  unmatch :: r -> Bool

class IsDone r where
  isDone :: r -> Bool

class (Num a) => HasPoints c a where
  pointsValue :: c -> a

instance IsDone (Result Char) where
  isDone Done = True
  isDone _ = False

instance HasPoints Char Integer where
  pointsValue ')' = 1
  pointsValue ']' = 2
  pointsValue '}' = 3
  pointsValue '>' = 4

instance HasNoMatch (Result Char) where
  unmatch (Unmatched _) = True
  unmatch _ = False

instance HasPoints Char Int where
  pointsValue ')' = 3
  pointsValue ']' = 57
  pointsValue '}' = 1197
  pointsValue '>' = 25137

flip' '(' = ')'
flip' '[' = ']'
flip' '{' = '}'
flip' '<' = '>'

openers = ['(', '[', '{', '<']

closers = [')', ']', '}', '>']

opener = flip elem openers

closer = flip elem closers
