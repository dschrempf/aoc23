-- |
-- Module      :  Main
-- Description :  Day 1
-- Copyright   :  2023 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Mon Nov  6 05:48:34 2023.
--
-- See https://adventofcode.com/2023/day/1.
module Main
  ( main,
  )
where

import Aoc
import Control.Applicative
import Data.Attoparsec.Text (Parser, decimal, endOfLine, sepBy1', space)
import Data.List (sortBy)
import Data.Ord (Down (Down), comparing)

pElf :: Parser [Int]
pElf = decimal `sepBy1'` endOfLine

pElfs :: Parser [[Int]]
pElfs = pElf `sepBy1'` some space

solve :: [[Int]] -> Int
solve = maximum . map sum

solve2 :: [[Int]] -> Int
solve2 = sum . take 3 . sortBy (comparing Data.Ord.Down) . map sum

main :: IO ()
main = do
  d <- parseChallengeT (F 1) pElfs
  let s = solve d
  print s
  let s2 = solve2 d
  print s2
