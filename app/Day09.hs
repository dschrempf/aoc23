-- |
-- Module      :  Main
-- Description :  Day 9
-- Copyright   :  2023 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Mon Nov  6 05:48:34 2023.
--
-- See https://adventofcode.com/2023/day/9.
module Main
  ( main,
  )
where

import Aoc (parseChallengeT)
import Aoc.Def (Challenge (..))
import Data.Attoparsec.Text
  ( Parser,
    char,
    decimal,
    endOfInput,
    endOfLine,
    sepBy1',
    signed,
  )
import Data.List (singleton)

diffs :: [Int] -> [Int]
diffs (x : y : zs) = (y - x) : diffs (y : zs)
diffs [_] = []
diffs [] = error "diffs: empty list"

diffss :: [Int] -> [[Int]]
diffss = go . singleton
  where
    go (xs : xss)
      | all (== 0) xs = xs : xss
      | otherwise = go $ diffs xs : xs : xss
    go [] = error "diffss: empty list"

next :: [Int] -> Int
next = sum . map last . diffss

pLine :: Parser [Int]
pLine = signed decimal `sepBy1'` char ' '

pInput :: Parser [[Int]]
pInput = pLine `sepBy1'` endOfLine <* endOfLine <* endOfInput

prev :: [Int] -> Int
prev = res . map head . reverse . diffss
  where
    res = sum . zipWith weigh [0 ..]
    weigh :: Int -> Int -> Int
    weigh n x
      | even n = x
      | otherwise = -x

main :: IO ()
main = do
  d <- parseChallengeT (Full 9) pInput
  print $ sum $ map next d
  print $ sum $ map prev d
