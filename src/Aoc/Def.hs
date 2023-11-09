-- |
-- Module      :  Aoc.Def
-- Description :  Definitions
-- Copyright   :  2023 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Thu Nov  9 09:58:58 2023.
module Aoc.Def
  ( Day,
    Challenge (..),
    getInputFile,
  )
where

import Numeric.Natural (Natural)
import Text.Printf (printf)

type Day = Natural

data Challenge
  = -- | Sample.
    S Day
  | -- | Full.
    F Day

getInputFile :: Challenge -> String
getInputFile (S n) = printf "inputs/day%02dsmpl.txt" n
getInputFile (F n) = printf "inputs/day%02dfull.txt" n
