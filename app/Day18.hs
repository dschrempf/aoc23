{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Main
-- Description :  Day 18
-- Copyright   :  2023 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Mon Nov  6 05:48:34 2023.
--
-- See https://adventofcode.com/2023/day/18.
module Main
  ( main,
  )
where

import Aoc
import Aoc.Direction (Direction (..))
import Aoc.Parse (skipHorizontalSpace)
import Data.Attoparsec.Text
  ( Parser,
    char,
    choice,
    decimal,
    endOfInput,
    endOfLine,
    hexadecimal,
    sepBy1',
    string,
  )
import Data.Word (Word8)
import GHC.Natural (Natural)

type RGB = (Word8, Word8, Word8)

data Instruction = Instruction {direction :: Direction, edgeLength :: Natural, rgb :: RGB}
  deriving (Eq, Show)

type DigPlan = [Instruction]

pDirection :: Parser Direction
pDirection =
  choice
    [ North <$ char 'U',
      East <$ char 'R',
      South <$ char 'D',
      West <$ char 'L'
    ]

pColor :: Parser RGB
pColor = do
  _ <- string "(#"
  c <- hexadecimal :: Parser Int
  _ <- char ')'
  let b = c `mod` 256
      g = ((c - b) `quot` 256) `mod` 256
      r = ((c - b) `quot` (256 * 256)) `mod` 256
  pure (fromIntegral r, fromIntegral g, fromIntegral b)

pInstruction :: Parser Instruction
pInstruction = do
  d <- pDirection
  _ <- skipHorizontalSpace
  l <- decimal
  _ <- skipHorizontalSpace
  Instruction d l <$> pColor

pDigPlan :: Parser DigPlan
pDigPlan = pInstruction `sepBy1'` endOfLine <* endOfLine <* endOfInput

main :: IO ()
main = do
  d <- parseChallengeT (Sample 18 1) pDigPlan
  print d
