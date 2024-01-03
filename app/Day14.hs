{-# LANGUAGE DeriveGeneric #-}
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
import Aoc.Array (parseMatrix, rotateLeft, rotateRight)
import Aoc.Definitions
import Aoc.Function (nTimesStrict)
import Aoc.List (findCycle, findFirstDuplicate)
import Aoc.Monad ((<.>))
import Control.DeepSeq (NFData)
import Control.Monad (foldM_, (>=>))
import Control.Monad.ST (runST)
import Data.Attoparsec.Text (Parser)
import Data.List (singleton)
import Data.Massiv.Array
  ( Array,
    B (..),
    Dimension (..),
    Ix1,
    Ix2,
    MArray,
    Manifest,
    PrimMonad (..),
  )
import qualified Data.Massiv.Array as A
import Data.Maybe (fromJust)
import GHC.Generics

data Position = Rolling | Fixed | Empty
  deriving (Eq, Ord, Generic)

instance NFData Position

charMap :: Position -> Char
charMap Rolling = 'O'
charMap Fixed = '#'
charMap Empty = '.'

instance Show Position where
  show = singleton . charMap

type Field = Array B Ix2 Position

pField :: Parser Field
pField = parseMatrix [('O', Rolling), ('#', Fixed), ('.', Empty)]

rollStonesToLastFixed ::
  (Manifest r Position, PrimMonad m, A.MonadThrow m) =>
  MArray (PrimState m) r Ix1 Position ->
  Ix1 ->
  Ix1 ->
  m Ix1
rollStonesToLastFixed xs lastFixed current
  | lastFixed == current = pure current
  | otherwise = do
      x <- A.readM xs current
      case x of
        Rolling -> do
          let lastFixed' = succ lastFixed
          A.writeM xs current Empty
          A.writeM xs lastFixed' Rolling
          pure lastFixed'
        Fixed -> pure current
        Empty -> pure lastFixed

tilt1D :: (Manifest r Position) => Array r Ix1 Position -> Array r Ix1 Position
tilt1D field = runST $ do
  fieldM <- A.thawS field
  foldM_ (rollStonesToLastFixed fieldM) (-1) rng
  A.freezeS fieldM
  where
    rng = A.imap const field

load1D :: Array B Ix1 Position -> Int
load1D = A.sum . A.imap f . A.reverse Dim1
  where
    f i Rolling = succ i
    f _ _ = 0

tiltAndComputeLoad :: Array B Ix2 Position -> Int
tiltAndComputeLoad = A.sum . A.map (load1D . tilt1D . A.computeAs B) . A.innerSlices

tiltNorth,
  tiltWest,
  tiltSouth,
  tiltEast ::
    (A.MonadThrow m) =>
    Array B Ix2 Position ->
    m (Array B Ix2 Position)
tiltWest = A.compute <.> A.stackOuterSlicesM . A.map tilt1D . A.outerSlices
tiltNorth = rotateRight <.> tiltWest . rotateLeft
tiltSouth = rotateLeft <.> tiltWest . rotateRight
tiltEast = (A.compute . A.reverse Dim1) <.> tiltWest . A.compute . A.reverse Dim1

cycleAoc' ::
  (A.MonadThrow m) =>
  Array B Ix2 Position ->
  m (Array B Ix2 Position)
cycleAoc' = tiltNorth >=> tiltWest >=> tiltSouth >=> tiltEast

cycleAoc :: Array B Ix2 Position -> Array B Ix2 Position
cycleAoc = fromJust . cycleAoc'

computeLoad :: Array B Ix2 Position -> Int
computeLoad = A.sum . A.map (load1D . A.computeAs B) . A.innerSlices

main :: IO ()
main = do
  f <- parseChallengeT (Full 14) pField
  print $ tiltAndComputeLoad f
  let fields = iterate cycleAoc f
  let cycleStart = fromJust $ findFirstDuplicate fields
  let (cycleLength, _) = fromJust $ findCycle 100 2 $ drop cycleStart fields
  let modulo = (1000000000 - cycleStart) `mod` cycleLength
  print $ computeLoad $ nTimesStrict (cycleStart + modulo) cycleAoc f
