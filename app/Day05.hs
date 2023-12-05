{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Main
-- Description :  Day 5
-- Copyright   :  2023 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Mon Nov  6 05:48:34 2023.
--
-- See https://adventofcode.com/2023/day/5.
module Main
  ( main,
  )
where

import Aoc (Challenge (..), parseChallengeT)
import Data.Attoparsec.Text
  ( Parser,
    decimal,
    endOfLine,
    isEndOfLine,
    sepBy1',
    skipSpace,
    skipWhile,
    string,
  )

type Seeds = [Int]

pSeeds :: Parser Seeds
pSeeds = string "seeds:" *> skipSpace *> decimal `sepBy1'` skipSpace

data MapEntry = MapEntry {destination :: Int, source :: Int, range :: Int}
  deriving (Show)

type Map = [MapEntry]

pMapEntry :: Parser MapEntry
pMapEntry = MapEntry <$> (decimal <* skipSpace) <*> (decimal <* skipSpace) <*> decimal

skipRestOfLine :: Parser ()
skipRestOfLine = skipWhile (not . isEndOfLine) >> endOfLine

pMap :: Parser Map
pMap = skipRestOfLine *> pMapEntry `sepBy1'` endOfLine

pInput :: Parser (Seeds, [Map])
pInput = do
  s <- pSeeds
  endOfLine
  endOfLine
  ms <- pMap `sepBy1'` (endOfLine <* endOfLine)
  return (s, ms)

main :: IO ()
main = do
  d <- parseChallengeT (Sample 5 1) pInput
  print $ fst d
  mapM_ print $ snd d
