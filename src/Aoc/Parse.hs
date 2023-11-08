-- |
-- Module      :  Main
-- Description :  Helper functions for parsing the input
-- Copyright   :  2023 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Wed Nov  8 12:21:12 2023.
module Aoc.Parse
  ( parse,
  )
where

import Data.Attoparsec.ByteString.Char8 (Parser, parseOnly)
import qualified Data.ByteString.Char8 as BS
import Data.Functor

parse :: FilePath -> Parser a -> IO a
parse file parser = BS.readFile file <&> either error id . parseOnly parser
