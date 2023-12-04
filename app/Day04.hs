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

data Card = Card {number :: Int, multiplicity :: Int, wins :: S.Set Int, picks :: S.Set Int}
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
  return $ Card n 1 (S.fromList ws) (S.fromList ps)

pInput :: Parser [Card]
pInput = pCard `sepBy1'` endOfLine <* endOfLine <* endOfInput

winningPicks :: Card -> S.Set Int
winningPicks (Card _ _ ws ps) = S.intersection ws ps

gradeIntersectionPart1 :: S.Set Int -> Int
gradeIntersectionPart1 xs
  | null xs = 0
  | otherwise = 2 ^ pred (S.size xs)

type Hand = [Card]

gradeCardPart2 :: Card -> Int
gradeCardPart2 = S.size . winningPicks

addNToCardI :: Int -> Hand -> Int -> Hand
addNToCardI n xs i = take i xs ++ [Card num (mul + n) ws ps] ++ drop (succ i) xs
  where
    (Card num mul ws ps) = xs !! i

gradeI :: Hand -> Int -> Hand
gradeI xs i =
  foldl (addNToCardI mul) xs $
    filter (< length xs) $
      take nWins [succ i ..]
  where
    cardToGrade = xs !! i
    mul = multiplicity cardToGrade
    nWins = gradeCardPart2 cardToGrade

gradeAll :: Hand -> Hand
gradeAll xs = foldl gradeI xs [0 .. (pred $ length xs)]

main :: IO ()
main = do
  d <- parseChallengeT (Full 4) pInput
  print $ sum $ map (gradeIntersectionPart1 . winningPicks) d
  print $ sum $ map multiplicity $ gradeAll d
