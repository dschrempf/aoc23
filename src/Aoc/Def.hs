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
  ( Year,
    year,
    Day,
    Challenge (..),
    getInputFile,
  )
where

import Numeric.Natural (Natural)
import Text.Printf (printf)

type Year = Natural

year :: Year
year = 2023

type Day = Natural

data Challenge
  = -- | Sample one.
    S1 Day
  | -- | Sample two
    S2 Day
  | -- | Full.
    F Day

getInputFile :: Challenge -> String
getInputFile (S1 n) = printf "inputs/day%02dsample1.txt" n
getInputFile (S2 n) = printf "inputs/day%02dsample2.txt" n
getInputFile (F n) = printf "inputs/day%02dfull.txt" n
