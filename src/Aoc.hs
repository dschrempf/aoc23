-- |
-- Module      :  Aoc
-- Description :  Advent of code library barrel file
-- Copyright   :  2023 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Sun Nov 12 08:59:52 2023.
module Aoc
  ( Day,
    Challenge (..),
    parseChallengeB,
    ParserB,
    parseChallengeT,
    ParserT,
  )
where

import Aoc.Def (Challenge (..), Day)
import Aoc.Parse (parseChallengeB, parseChallengeT)
import qualified Data.Attoparsec.ByteString.Char8 as AB
import qualified Data.Attoparsec.Text as AT
import Data.ByteString.Char8 (ByteString)
import Data.Text (Text)

type ParserT = AT.Parser Text

type ParserB = AB.Parser ByteString
