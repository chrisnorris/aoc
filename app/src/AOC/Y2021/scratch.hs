#!/usr/bin/env stack
{- stack --resolver lts-13.19 --install-ghc
      exec ghci
      --package regex-tdfa
      --package lens
      --package parsec
      --package data-clist
      --package filepath
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

import Control.Lens as L
import Control.Monad (ap, foldM, forM, forM_, liftM2)
import Lib
import qualified Data.Array as A
import Data.Bits
import Data.Bits.Lens as L
import qualified Data.CircularList as CL
import Data.Either (fromRight)
import Data.List (drop, intersect, intersperse, nub, sort, sortBy, take, unfoldr)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Monoid (Last (..))
import qualified Data.Set as Set

import System.FilePath.Posix
import System.IO

import qualified Text.Parsec as P
import Text.Regex.TDFA

main = do
  let m inp = sum [1 | (f,s) <- inp `zip` (tail inp) , f < s ]
  map read <$> (lines <$> readFile21 "d1.input") >>= return . liftM2 (,) m (m . (sum <$>) . win)

main2 = do
  let m inp = length . filter (== LT) $ uncurry compare <$> inp `zip` (drop 1 inp <> [0])
  map read <$> (lines <$> readFile21 "d1.input") >>= return . liftM2 (,) m (m . (sum <$>) . win)

readFile21 :: FilePath -> IO String
readFile21 = readFile . ("../../../input/2021" </>)


win = go []
 where go l [] = l
       go l xs = go (l <> [take 3 xs]) (drop 1 xs)