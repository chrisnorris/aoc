module AOC.Y2022.Day11 where

import AOC.Y2021.Day24 (integer)
import Control.Arrow ((&&&))
import Control.Monad.State.Lazy as SL
import Data.Map as Map
import Data.Set (fromList, size)
import Library hiding (fromList, size)
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim

data Terminal where
  Add :: Integer -> Terminal
  Noop :: Terminal

main_pt1 = do
  input <- readFileY 2022 "d11.input.sam"
  let t = sequence (parse (many1 monkeys) "mkys" input)
  return $ sequence t ^. _Right

monkey = do
  skipMany (letter <|> space)
  i <- integer
  char ':'
  return i

starting = do
  manyTill anyChar (char ':')
  concat <$> many integer `sepBy` string ", "

operation = do
  manyTill anyChar (char '=')
  sepBy1 space (string "old")
  liftM2
    (,)
    (oneOf ['+', '*'])
    (try (I <$> integer) <|> try (Old <$> (many space >> string "old" >> pure ())))

data Op a = I Integer | Old a deriving (Show)

tests = do
  skipMany (letter <|> space <|> char ':')
  db <- integer
  (db,) <$> liftM2 (,) (boolChk "true") (boolChk "false")

boolChk b = do
  sepBy (many space) (string "If")
  string b
  skipMany (letter <|> char ':' <|> space)
  integer

data Monkey = Monkey
  { m1 :: Integer,
    items :: [Integer],
    oprtn :: (Char, Op ()),
    leveltest :: (Integer, (Integer, Integer))
  }
  deriving (Show)

monkeys = Monkey <$> monkey <*> starting <*> operation <*> tests

aroundo = do
  notes <- main_pt1
  let monkeys = (Map.fromList ([0 ..] `zip` notes))
      counts = (Map.fromList ([0 .. 3] `zip` [0, 0, 0, 0]))
  return $ scanl (\m _ -> (execState (around 3)) m) (monkeys, counts) [0 .. 20]

around :: Integer -> SL.State ((Map Integer Monkey), (Map Integer Int)) Integer
around x = do
  forM_ [0 .. x] $ \i -> do
    mmap <- fst <$> get
    let Just rule = mmap ^. at i
    -- counts <- snd <$> get
    forM_ (items rule) $ \item -> do
      -- let a = length $ items rule
      -- modify $ \ (a,c) -> (a, ix i %~ ((+) 0) $ c )
      mmapItems <- fst <$> get
      let (wlvl, recipient) = throwTo item rule
      let Just thwToItems = mmapItems ^. at recipient
          recipientItems = items thwToItems
      modify $ \(a, b) -> ((at recipient ?~ (thwToItems {items = recipientItems <> [wlvl]}) $ mmapItems), ix recipient %~ ((+) 1) $ b)
    modify $ \(a, b) -> (over (at i) (thrownAll <$>) a, b)

  return 0

thrownAll :: Monkey -> Monkey
thrownAll m@Monkey {..} = m {items = []}

throwTo item rule@Monkey {..} =
  let (divisibility, (t, f)) = leveltest
      r = case oprtn of
        (c, Old ()) -> fromOp c item item
        (c, I x) -> fromOp c item x
      wl = (floor $ fromInteger r / 3)
   in (wl,) (if mod wl divisibility == 0 then t else f)
  where
    fromOp = \case
      '*' -> (*)
      '+' -> (+)

testy mm = (\(x, y) -> throwTo x ((!) mm y)) <$> ((,2) <$> [79, 60, 97])

-- (\m -> length . concat $ items . (flip (!) m) <$> aa) <$> [0..3]
