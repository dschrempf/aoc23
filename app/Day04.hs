{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Main
-- Description :  Day 4
-- Copyright   :  2023 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Mon Nov  6 05:48:34 2023.
--
-- See https://adventofcode.com/2023/day/4.
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
    skipSpace,
    string,
  )
import qualified Data.Set as S

data Card = Card {number :: Int, wins :: S.Set Int, picks :: S.Set Int}
  deriving (Show)

pCard :: Parser Card
pCard = do
  _ <- string "Card"
  skipSpace
  n <- decimal
  _ <- char ':'
  skipSpace
  ws <- decimal `sepBy1'` skipSpace
  skipSpace
  _ <- char '|'
  skipSpace
  ps <- decimal `sepBy1'` skipSpace
  return $ Card n (S.fromList ws) (S.fromList ps)

pInput :: Parser [Card]
pInput = pCard `sepBy1'` endOfLine <* endOfLine <* endOfInput

winningPicks :: Card -> S.Set Int
winningPicks (Card _ ws ps) = S.intersection ws ps

gradeIntersectionPart1 :: S.Set Int -> Int
gradeIntersectionPart1 xs
  | null xs = 0
  | otherwise = 2 ^ pred (S.size xs)
main :: IO ()
main = do
  d <- parseChallengeT (Full 4) pInput
  print $ sum $ map (gradeIntersectionPart1 . winningPicks) d
