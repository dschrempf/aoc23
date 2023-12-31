-- |
-- Module      :  Main
-- Description :  Day 16
-- Copyright   :  2023 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Mon Nov  6 05:48:34 2023.
--
-- See https://adventofcode.com/2023/day/16.
module Main
  ( main,
  )
where

import Aoc
import Aoc.Array (pMatrix)
import Data.Attoparsec.Text (Parser)
import Data.Massiv.Array (Array, B (..), Ix2 (..))

data Tile = Empty | MirrorSlash | MirrorBackSlash | SplitterV | SplitterH

instance Show Tile where
  show Empty = "."
  show MirrorSlash = "/"
  show MirrorBackSlash = "\\"
  show SplitterV = "|"
  show SplitterH = "-"

type Contraption = Array B Ix2 Tile

pContraption :: Parser Contraption
pContraption =
  pMatrix
    [ ('.', Empty),
      ('/', MirrorSlash),
      ('\\', MirrorBackSlash),
      ('|', SplitterV),
      ('-', SplitterH)
    ]

main :: IO ()
main = do
  d <- parseChallengeT (Sample 16 1) pContraption
  print d
