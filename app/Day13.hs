-- |
-- Module      :  Main
-- Description :  Day 13
-- Copyright   :  2023 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Mon Nov  6 05:48:34 2023.
--
-- See https://adventofcode.com/2023/day/13.
module Main
  ( main,
  )
where

import Aoc (Challenge (..), parseChallengeT)
import Aoc.Array (pMatrix)
import Data.Attoparsec.Text (Parser, endOfInput, endOfLine, sepBy1', skipSpace)
import Data.Massiv.Array (Array, B, Ix2)

data Point = Ash | Rock
  deriving (Eq)

instance Show Point where
  show Ash = "."
  show Rock = "#"

type Field = Array B Ix2 Point

pField :: Parser Field
pField = pMatrix [('.', Ash), ('#', Rock)]

pInput :: Parser [Field]
pInput = pField `sepBy1'` skipSpace <* endOfLine <* endOfInput

main :: IO ()
main = do
  f <- parseChallengeT (Sample 13 1) pInput
  print f
