module AOC.Y2021.Day24 where

import           Library       
import qualified Data.Map  as M            
import Control.Monad.Trans.State.Strict (State, modify)
import Prelude hiding (mod)
import Text.Parsec
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Expr as Ex

main = main24 Full >>= print

main24 source = do

  source <- map parseExpr <$> getInput source
  
  -- return "done"

  let startEnv d = updateEnv (foldl zeroEnv' emptyEnv (Name <$> ["s", "w", "x", "y", "z"]))
                     (Name "model") (VModel d)
  let result = foldl evals (startEnv [4]) source

  print result
  return source

 where
  getInput source = inp21Str ("d24.input" <> show source)
  zeroEnv' :: Env -> Name -> Env
  zeroEnv' (Env m) x = Env (M.insert x (VNum 0) m)

evals :: Env -> Either ParseError Expr -> Env
evals env = \case
    Left s -> emptyEnv
    Right expr -> eval' env expr


eval' :: Env -> Expr -> Env
eval' env = 
  \case
    Inp (Var v) -> let (VModel model) = lookupEnv env (Name "model")
                       pc = 0 in
                   updateEnv env (Name v) (VNum $ model!!pc)

    BinOp op (Var v1) (Int i2) -> let (VNum ev1) = lookupEnv env (Name v1) in
                                     updateEnv env (Name v1) (VNum $ truncate $ asOp op (fromIntegral ev1) (fromIntegral i2))

    BinOp op (Var v1) (Var v2) -> let (VNum ev1) = lookupEnv env (Name v1)
                                      (VNum ev2) = lookupEnv env (Name v2) in
                                      updateEnv env (Name v1) (VNum $ truncate $ asOp op (fromIntegral ev1) (fromIntegral ev2))

    Mod (Var v1) (Var v2) ->  let (VNum ev1) = lookupEnv env (Name v1) 
                                  (VNum ev2) = lookupEnv env (Name v2) in
                              updateEnv env (Name v1) (VNum $ rem ev1 ev2)

    Mod (Var v1) (Int i2) ->  let (VNum ev1) = lookupEnv env (Name v1) in
                              updateEnv env (Name v1) (VNum $ rem ev1 i2)

    Eql (Var v1) (Var v2) ->  let (VNum ev1) = lookupEnv env (Name v1)
                                  (VNum ev2) = lookupEnv env (Name v2) in
                              updateEnv env (Name v1) (if ev1 == ev2 then VNum 1 else VNum 0)

    Eql (Var v1) (Int ev2) -> let (VNum ev1) = lookupEnv env (Name v1) in
                              updateEnv env (Name v1) (if ev1 == ev2 then VNum 1 else VNum 0)

    _ -> undefined

eval :: Env -> Expr -> Val
eval env = 
  \case
     Inp (Var v) -> let env' = updateEnv env (Name v) (VNum 2) in lookupEnv env' (Name v)
 -- rather than just updating this needs to proceed somehow
     _ -> undefined

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
    | VError String deriving (Eq, Ord, Show)

newtype Env' = Env' (M.Map Name Val) deriving Show
data Env = Env {pc :: Int,  env :: (M.Map Name Val)} deriving Show

emptyEnv :: Env
emptyEnv = Env M.empty

lookupEnv :: Env -> Name -> Val
lookupEnv (Env m) x = fromMaybe (VNum 0) (M.lookup x m)

updateEnv :: Env -> Name -> Val -> Env
updateEnv Env{..} x v = Env { pc = pc+1, env = (M.insert x v env)}

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
    names = ["inp", "add", "div", "mul","mod", "eql"]
    style = emptyDef { Tok.reservedNames = names }

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
factor = try inp
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

parseExpr :: String -> Either ParseError Expr
parseExpr = parse (contents expr) "<stdin>"

newtype Name = Name String  deriving (Eq, Ord, Show)
