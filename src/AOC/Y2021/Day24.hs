{-# LANGUAGE BangPatterns #-}

module AOC.Y2021.Day24 where

import Control.Monad
import Data.Char (digitToInt)
import qualified Data.Map as M
import Library
import Text.Parsec
import qualified Text.Parsec.Expr as Ex
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok
import Prelude hiding (mod)

main = main24 Full

main24 source = do
  s <- map parseExpr <$> getInput source
  let m1 = (toInteger . digitToInt <$>) "99999999999999"
  let initEnv = updateEnv' e1 (Name "m") (VModel m1)

  let s0 = MState initEnv s

  print $ highest s0 0
  where
    getInput source = inp21Str ("d24.input" <> show source)
    validate e =
      let VNum z = lookupEnv e (Name "z") in if z == 0 then Valid else Invalid

    highest s n | n > (10 ^ 9) = (Invalid, s)
    highest s@MState {..} n =
      let !result = validate $ foldl' evals startEnv source
       in if result == Valid then (Valid, s) else highest (nextEnv s) (n + 1)

    nextEnv e@MState {..} =
      let (VModel oldModel) = lookupEnv startEnv (Name "m")
       in MState {startEnv = updateEnv' e1 (Name "m") (VModel $ dec oldModel), ..}

{-
Here is an ALU program which takes a non-negative integer as input, converts it into binary,
and stores the lowest (1's) bit in z, the second-lowest (2's) bit in y, the third-lowest (4's) bit in x,
and the fourth-lowest (8's) bit in w 1000
-}
test1 = do
  s <- map parseExpr <$> inp21Str "d24T1.input"
  let m1 = (toInteger . digitToInt <$>) "9"
  let initEnv = updateEnv' e1 (Name "m") (VModel m1)
  let s0 = MState initEnv s
  return $ assertWith s0 == [1, 0, 0, 1]
  where
    assertWith s@MState {..} =
      let env = foldl' evals startEnv source
       in (\(VNum v) -> v) . lookupEnv env . Name <$> reg

newtype Env' = Env' (M.Map Name Val) deriving (Show)

newtype Name = Name String deriving (Eq, Ord, Show)

data MState = MState
  { startEnv :: Env,
    source :: [ParseResult]
  }
  deriving (Show)

data Expr
  = Inp Expr
  | Int Integer
  | Float Double
  | BinOp Op Expr Expr
  | Mod Expr Expr
  | Var String
  | Eql Expr Expr
  deriving (Eq, Ord, Show)

data Val
  = VNum Integer
  | VStr String
  | VModel [Integer]
  | VError String
  deriving (Eq, Ord, Show)

data Env = Env
  { pc :: Int,
    env :: M.Map Name Val
  }
  deriving (Show)

data ModelNumber = Valid | Invalid deriving (Eq, Show)

type ParseResult = Either ParseError Expr

e1 :: Env
e1 = foldl' zeroEnv' emptyEnv (Name <$> reg)

zeroEnv' :: Env -> Name -> Env
zeroEnv' Env {..} x = Env {env = M.insert x (VNum 0) env, ..}

reg :: [String]
reg = ["w", "x", "y", "z"]

dec :: [Integer] -> [Integer]
dec (l : ls) = let m = (l - 1) in aux m
  where
    aux m
      | m == 0 = 9 : dec ls
      | otherwise = m : ls

evals :: Env -> Either ParseError Expr -> Env
evals env = \case
  Left s -> emptyEnv
  Right expr -> eval' env expr

eval' :: Env -> Expr -> Env
eval' e@Env {..} = \case
  Inp (Var v) ->
    let (VModel model) = lookupEnv e (Name "m")
     in updateEnv e (Name v) (VNum $ model !! pc)
  BinOp op (Var v1) (Int i2) ->
    let (VNum ev1) = lookupEnv e (Name v1)
     in updateEnv'
          e
          (Name v1)
          (VNum $ truncate $ asOp op (fromIntegral ev1) (fromIntegral i2))
  BinOp op (Var v1) (Var v2) ->
    let (VNum ev1) = lookupEnv e (Name v1)
        (VNum ev2) = lookupEnv e (Name v2)
     in updateEnv'
          e
          (Name v1)
          (VNum $ truncate $ asOp op (fromIntegral ev1) (fromIntegral ev2))
  Mod (Var v1) (Var v2) ->
    let ev1 = retrieve e v1
        ev2 = retrieve e v2
     in updateEnv' e (Name v1) (VNum $ rem ev1 ev2)
  Mod (Var v1) (Int i2) ->
    let ev1 = retrieve e v1 in updateEnv' e (Name v1) (VNum $ rem ev1 i2)
  Eql (Var v1) (Var v2) ->
    let ev1 = retrieve e v1
        (VNum ev2) = lookupEnv e (Name v2)
     in updateEnv' e (Name v1) (if ev1 == ev2 then VNum 1 else VNum 0)
  Eql (Var v1) (Int ev2) ->
    let ev1 = retrieve e v1
     in updateEnv' e (Name v1) (if ev1 == ev2 then VNum 1 else VNum 0)
  _ -> undefined
  where
    retrieve e v = let VNum ev = lookupEnv e (Name v) in ev

emptyEnv :: Env
emptyEnv = Env {pc = 0, env = M.empty}

lookupEnv :: Env -> Name -> Val
lookupEnv Env {..} n = fromMaybe (VNum 0) (M.lookup n env)

updateEnv :: Env -> Name -> Val -> Env
updateEnv Env {..} x v = Env {pc = pc + 1, env = M.insert x v env}

updateEnv' :: Env -> Name -> Val -> Env
updateEnv' Env {..} x v = Env {env = M.insert x v env, ..}

data Op
  = Plus
  | Minus
  | Times
  | Divide
  deriving (Eq, Ord, Show)

class FromOp b where
  asOp :: (Num a, Fractional a) => b -> a -> a -> a

instance FromOp Op where
  asOp Plus = (+)
  asOp Minus = (-)
  asOp Times = (*)
  asOp Divide = (/)

table = []

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where
    names = ["inp", "add", "div", "mul", "mod", "eql"]
    style = emptyDef {Tok.reservedNames = names}

integer :: Parser Integer
integer = Tok.integer lexer

float :: Parser Double
float = Tok.float lexer

identifier :: Parser String
identifier = Tok.identifier lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

int :: Parser Expr
int = Int <$> integer

floating :: Parser Expr
floating = Float <$> float

expr :: Parser Expr
expr = Ex.buildExpressionParser [] factor

variable :: Parser Expr
variable = Var <$> identifier

binOp :: Op -> String -> Parser Expr
binOp op r = do
  reserved r
  var <- identifier
  BinOp op (Var var) . Var <$> identifier

binOp' :: Op -> String -> Parser Expr
binOp' op r = do
  reserved r
  var <- identifier
  BinOp op (Var var) <$> int

add :: Parser Expr
add = binOp Plus "add"

add' :: Parser Expr
add' = binOp' Plus "add"

mul :: Parser Expr
mul = binOp Times "mul"

mul' :: Parser Expr
mul' = binOp' Times "mul"

divP :: Parser Expr
divP = binOp Divide "div"

divP' :: Parser Expr
divP' = binOp' Divide "div"

inp :: Parser Expr
inp = do
  reserved "inp"
  Inp . Var <$> identifier

mod :: Parser Expr
mod = do
  reserved "mod"
  var <- identifier
  Mod (Var var) <$> int

eql :: Parser Expr
eql = do
  reserved "eql"
  varA <- identifier
  Eql (Var varA) . Var <$> identifier

eql' :: Parser Expr
eql' = do
  reserved "eql"
  varA <- identifier
  Eql (Var varA) <$> int

factor :: Parser Expr
factor =
  try inp
    <|> try add
    <|> try add'
    <|> try mul
    <|> try mul'
    <|> try divP
    <|> try divP'
    <|> try mod
    <|> try eql'
    <|> try eql

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

parseExpr :: String -> ParseResult
parseExpr = parse (contents expr) "<stdin>"
