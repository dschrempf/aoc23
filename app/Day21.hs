-- |
-- Module      :  Main
-- Description :  Day 21
-- Copyright   :  2023 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Mon Nov  6 05:48:34 2023.
--
-- See https://adventofcode.com/2023/day/21.
module Main
  ( main,
  )
where

import Aoc
import Aoc.Array (parseMatrix)
import Data.Attoparsec.Text (Parser)
import Data.Massiv.Array (Array, B, Ix2)
import qualified Data.Massiv.Array as A
import Data.Set (Set)
import qualified Data.Set as S

type Rocks = Set Ix2

type Positions = Set Ix2

data Tile = Start | Plot | Rock
  deriving (Show)

pInput :: Parser (Array B Ix2 Tile)
pInput = parseMatrix [('S', Start), ('.', Plot), ('#', Rock)]

getRocks :: Array B Ix2 Tile -> Rocks
getRocks = A.ifoldlS addRock S.empty
  where
    addRock rocks ix Rock = S.insert ix rocks
    addRock rocks _ _ = rocks

getStart :: Array B Ix2 Tile -> Ix2
getStart = undefined

main :: IO ()
main = do
  d <- parseChallengeT (Sample 21 1) pInput
  print d
