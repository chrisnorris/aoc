module Library
  ( module Library
  , module L
  , module A
  , module CL
  , module P
  , module Data.Bits
  , module Data.List
  , module Text.Regex.TDFA
  , ap
  , forM_
  , forM
  , foldM
  , fromJust
  )
where

import           Control.Lens                  as L
                                         hiding ( noneOf )
import           Control.Monad                  ( ap
                                                , foldM
                                                , forM
                                                , forM_
                                                , liftM2
                                                )
import qualified Data.Array                    as A
import           Data.Bits
import           Data.Bits.Lens                as L
import           Data.CircularList             as CL
                                         hiding ( empty )
import           Data.Either                    ( fromRight )
import           Data.List                      ( drop
                                                , intersect
                                                , intersperse
                                                , nub
                                                , sort
                                                , sortBy
                                                , take
                                                , unfoldr
                                                )
import           Data.Map                      as Map
                                         hiding ( drop
                                                , foldr
                                                , foldl
                                                )
import           Data.Maybe                     ( fromJust )
import           Data.Monoid                    ( Last(..) )
import           System.FilePath.Posix
import           System.IO
import           Text.Parsec                   as P
                                         hiding ( uncons
                                                , Empty
                                                )
import           Text.Regex.TDFA

readFile' :: FilePath -> IO String
readFile' file = readFile ("app/2020/input" </> file)

for = flip Prelude.map
trimmed = drop 1

trimEnd = reverse . drop 2 . reverse

getIntersects (l : ls) = foldl intersect l ls

parseFile :: [String] -> String
parseFile = foldr newLineDemarcated []
 where
  newLineDemarcated e st = case e of
    "" -> st <> "\n"
    _  -> st <> " " <> e

data Ins = Nop Int | Acc Int | Jmp Int deriving (Show)

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

breakOut st el = [[el]] <> st

repeatN n = foldr (.) id . replicate n



