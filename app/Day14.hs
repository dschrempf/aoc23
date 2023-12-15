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
import Data.Attoparsec.Text (Parser)
import Data.List (singleton)
import Data.Massiv.Array (Array, B, Ix2)

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

main :: IO ()
main = do
  f <- parseChallengeT (Sample 14 1) pField
  print f
