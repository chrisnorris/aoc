module Library
  ( module Library
  , module L
  , module A
  , module CL
  , module P
  , module Data.Bits
  , module Data.List
  , module Data.List.Split
  , module Text.Regex.TDFA
  , ap
  , forM_
  , forM
  , foldM
  , fromJust
  , fromMaybe
  , void
  , liftM2
  , fst3
  )
where

import           Control.Lens                  as L
                                         hiding ( noneOf )
import           Control.Monad                  ( ap
                                                , foldM
                                                , forM
                                                , forM_
                                                , liftM2
                                                , void
                                                )
import qualified Data.Array                    as A
import           Data.Bits
import           Data.Bits.Lens                as L
import           Data.Tuple.Extra               (fst3)
import           Data.CircularList             as CL
                                         hiding ( empty )
import           Data.Either                    ( fromRight )
import           Data.List                      ( drop
                                                , intersect
                                                , intersperse
                                                , intercalate
                                                , nub
                                                , sort
                                                , sortBy
                                                , take
                                                , unfoldr
                                                , partition
                                                , group
                                                , foldl'
                                                ,(\\)
                                                )
import           Data.List.Split                (chunksOf)
import           Data.Map                      as Map
                                         hiding ( drop
                                                , foldr
                                                , foldl
                                                , take
                                                )
import           Data.Maybe                     ( fromMaybe
                                                , fromJust )
import           Data.Monoid                    ( Last(..) )
import           System.FilePath.Posix
import           System.IO
import           Text.Parsec                   as P
                                         hiding ( uncons
                                                , Empty
                                                )
import           Text.Regex.TDFA

type PairInts = (Int, Int)

data Source = Sample | Full

data Stack a = Stack [a]
  deriving (Show, Functor)

data Ins = Nop Int | Acc Int | Jmp Int deriving (Show)

instance Show Source where
        show Sample = ".sam"
        show Full = ""

inp21Str :: FilePath -> IO [String]
inp21Str = (lines <$>) . readFile . ("app/input/2021" </>)

readFile21 :: FilePath -> IO String
readFile21 = readFile . ("app/input/2021" </>)

readFile20 :: FilePath -> IO String
readFile20 = readFile . ("app/input/2020" </>)

buckets a [] = a 
buckets a x = buckets (take 1 x:a) (drop 1 x)

for = flip Prelude.map

trimmed = drop 1

trimEnd = reverse . drop 2 . reverse

getIntersects (l : ls) = foldl intersect l ls

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

debug :: Show a => a -> IO ()
debug = print

parseFile :: [String] -> String
parseFile = foldr newLineDemarcated []
 where
  newLineDemarcated e st = case e of
    "" -> st <> "\n"
    _  -> st <> " " <> e

parseOperand op = case words op of
  "jmp" : v : _ -> makeOp Jmp v
  "acc" : v : _ -> makeOp Acc v
  "nop" : v : _ -> makeOp Nop v

makeOp :: (Int -> Ins) -> String -> Ins
makeOp op v = case head v of
  '+' -> op (read (drop 1 v) :: Int)
  _   -> op (read v :: Int)

mkRegex :: String -> Regex
mkRegex = makeRegexOpts defaultCompOpt { multiline = False } defaultExecOpt

asCircularList :: String -> CList Int
asCircularList = CL.fromList . (read <$>) . reverse . foldl breakOut []

breakOut = flip ((<>) . return . return)

repeatN = (foldr (.) id .) . replicate

sempty :: Stack a
sempty = Stack []

push :: a -> Stack a -> Stack a
push x (Stack xs) = Stack (x : xs)

pop :: Stack a -> (Maybe a, Stack a)
pop (Stack []      ) = (Nothing, Stack [])
pop (Stack (x : xs)) = (Just x, Stack xs)
