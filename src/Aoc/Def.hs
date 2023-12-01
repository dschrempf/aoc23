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
    ChallengeType (..),
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

data ChallengeType
  = -- | Sample one.
    Sample1
  | -- | Sample two
    Sample2
  | -- | Full.
    Full

data Challenge = Challenge {challengeDay :: Day, challengeType :: ChallengeType}

getInputFile :: Challenge -> String
getInputFile (Challenge n Sample1) = printf "inputs/day%02dsample1.txt" n
getInputFile (Challenge n Sample2) = printf "inputs/day%02dsample2.txt" n
getInputFile (Challenge n Full) = printf "inputs/day%02dfull.txt" n
