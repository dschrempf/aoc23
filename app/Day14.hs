{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      :  Main
-- Description :  Day 14
-- Copyright   :  2023 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Mon Nov  6 05:48:34 2023.
--
-- See https://adventofcode.com/2023/day/14.
module Main
  ( main,
  )
where

import Aoc (parseChallengeT)
import Aoc.Array (pMatrix)
import Aoc.Def
import Control.Monad (when)
import Control.Monad.ST (runST)
import Data.Attoparsec.Text (Parser)
import Data.List (singleton)
import Data.Massiv.Array
  ( Array,
    B,
    Ix1,
    Ix2,
    MArray,
    Manifest,
    PrimMonad (..),
    Sz (..),
  )
import qualified Data.Massiv.Array as A

-- import qualified Data.Massiv.Array as A

data Position = Rolling | Fixed | Empty
  deriving (Eq)

charMap :: Position -> Char
charMap Rolling = 'O'
charMap Fixed = '#'
charMap Empty = '.'

instance Show Position where
  show = singleton . charMap

type Field = Array B Ix2 Position

pField :: Parser Field
pField = pMatrix [('O', Rolling), ('#', Fixed), ('.', Empty)]

tilt :: Array B Ix1 Position -> Array B Ix1 Position
tilt xs = runST $ do
  xsM <- A.thawS xs
  A.forM_ rng (pSwap xsM)
  A.freezeS xsM
  where
    rng = A.imap const xs

pSwap :: (Manifest r Position, PrimMonad m, A.MonadThrow m) => MArray (PrimState m) r Ix1 Position -> Ix1 -> m ()
pSwap ysM j = do
  a <- A.readM ysM j
  b <- A.readM ysM (succ j)
  when (a == Empty && b == Rolling) $ A.swap_ ysM j (succ j)

main :: IO ()
main = do
  f <- parseChallengeT (Sample 14 1) pField
  print f
