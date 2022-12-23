module AOC.Y2022.Day21 where

import           Library

import           Data.List                     as L
import           Data.Map                      as Map
import           Control.Lens
import           Text.Parsec
import           Text.Parsec.Prim
import           Text.Parsec.Char
import           Text.Parsec.Combinator
import           AOC.Y2021.Day24                ( integer )

-- 160274622817992
main_pt1 =
  parsedC >>=
    return . run (+)

-- 3087390115721
main_pt2 = do
  commands <- parsedC
  let (v2, n) = head [ (v, i) | (Yell "humn" v, i) <- commands `zip` [0 ..] ]
  let humnCommands = commands & ix n .~ Yell "humn" 1

  let loop :: (Integer -> IO Integer)
      loop v = do
        print $ "trying:" <> show v
        if run (==) (commands & ix n .~ Yell "humn" v)
          then return v
          else loop (v + 1)
  loop 3087390115721

parsedC = do
  x <- inpStr 2022 "d21.input"
  return $ sequence (parse command "monkeyness" <$> x) ^. _Right

run op commands = go commands Map.empty []
 where
  go []       m acc = go acc m []
  go (i : is) m acc = case i of
    Wait "root" (I a) (I b) -> a `op` b
    Yell mky no             -> go is (Map.insert mky no m) acc
    Wait   mky (I x) (I y)  -> go is (Map.insert mky (x + y) m) acc
    Minusy mky (I x) (I y)  -> go is (Map.insert mky (x - y) m) acc
    Mult   mky (I x) (I y)  -> go is (Map.insert mky (x * y) m) acc
    Div    mky (I x) (I y)  -> go is (Map.insert mky (x `div` y) m) acc
    Wait mky n1 n2 -> go is m (Wait mky (resolve m n1) (resolve m n2) : acc)
    Minusy mky n1 n2 ->
      go is m (Minusy mky (resolve m n1) (resolve m n2) : acc)
    Mult mky n1 n2 -> go is m (Mult mky (resolve m n1) (resolve m n2) : acc)
    Div  mky n1 n2 -> go is m (Div mky (resolve m n1) (resolve m n2) : acc)

main_pt22 = do
  x <- inpStr 2022 "d21.input"
  let commands     = sequence (parse command "monkeyness" <$> x) ^. _Right
  let (v2, n) = head [ (v, i) | (Yell "humn" v, i) <- commands `zip` [0 ..] ]
  let humnCommands = commands & ix n .~ Yell "humn" 1

  let loop
        :: Run -> Integer -> [(Ordering, Integer)] -> IO [(Ordering, Integer)]
      loop r@Run {..} v acc = if v > end
        then return $ reverse acc
        else print ("trying:" <> show v) >> loop
          r
          (v + steps r)
          ((run compare (commands & ix n .~ Yell "humn" v), v) : acc)

      arun r@Run {..} = loop r start []
      ans rr@Run {..} = do
        l <- arun rr
        print l
        let v = [ p | (GT, p) <- l ]
        case v of
          [] -> return 0
          _  -> ans $ Run (maximum v) end
  -- iterate on LT and GT until EQ found.
  ans (Run 3087390115602 3087390115729)

resolve m i@(I _) = i
resolve m (  J s) = case Map.lookup s m of
  Just x  -> I x
  Nothing -> J s

command = try yell <|> try wait <|> try minus <|> try mult <|> try divide

jobName = do
  m <- many1 letter
  string ":"
  many space
  return m

data Run = Run
  { start :: Integer
  , end   :: Integer
  }

steps :: Run -> Integer
steps Run {..} = round $ (fromIntegral end - fromIntegral start) / 2

data R = I Integer | J Monkey deriving Show

monkey = J <$> many1 letter

data Job where
  Yell ::Monkey -> Integer -> Job
  Wait ::Monkey -> R -> R -> Job
  Minusy ::Monkey -> R -> R -> Job
  Mult ::Monkey -> R -> R -> Job
  Div ::Monkey -> R -> R -> Job
  deriving Show

type Monkey = String

wait = job Plus Wait
minus = job Minus Minusy
mult = job Multiply Mult
divide = job Divide Div

job :: Action -> (Monkey -> R -> R -> a) -> ParsecT String () Identity a
job a c =
  c <$> jobName <*> monkey <*> (space *> string (show a) *> space *> monkey)

yell = Yell <$> jobName <*> try integer

data Action = Plus | Minus | Multiply | Divide

instance Show Action where
  show Plus     = "+"
  show Minus    = "-"
  show Multiply = "*"
  show Divide   = "/"
