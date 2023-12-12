-- |
-- Module      :  Main
-- Description :  Day 12
-- Copyright   :  2023 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Mon Nov  6 05:48:34 2023.
--
-- See https://adventofcode.com/2023/day/12.
module Main
  ( main,
  )
where

import Aoc
import Aoc.Parse (skipHorizontalSpace)
import Control.Applicative (Alternative (..))
import Data.Attoparsec.Text (Parser, char, choice, decimal, endOfInput, endOfLine, sepBy1')

data Spring = Operational | Damaged | Unknown
  deriving (Eq)

instance Show Spring where
  show Operational = "."
  show Damaged = "#"
  show Unknown = "?"

pSpring :: Parser Spring
pSpring = choice [o, d, u]
  where
    o = Operational <$ char '.'
    d = Damaged <$ char '#'
    u = Unknown <$ char '?'

type Springs = [Spring]

type Groups = [Int]

pGroups :: Parser Groups
pGroups = decimal `sepBy1'` char ','

type Line = (Springs, Groups)

pLine :: Parser Line
pLine = (,) <$> some pSpring <* skipHorizontalSpace <*> pGroups

pInput :: Parser [Line]
pInput = pLine `sepBy1'` endOfLine <* endOfLine <* endOfInput

main :: IO ()
main = do
  d <- parseChallengeT (Sample 12 2) pInput
  mapM_ print d
