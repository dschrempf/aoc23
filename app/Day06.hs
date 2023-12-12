{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Main
-- Description :  Day 6
-- Copyright   :  2023 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Mon Nov  6 05:48:34 2023.
--
-- See https://adventofcode.com/2023/day/6.
module Main
  ( main,
  )
where

import Aoc
import Data.Attoparsec.Text
  ( Parser,
    decimal,
    endOfInput,
    endOfLine,
    sepBy1',
    skipSpace,
    string,
  )

qu :: Int -> Int -> (Int, Int)
qu tm' sm' = (floor $ 0.5 * tm - c + 1, ceiling $ 0.5 * tm + c - 1)
  where
    tm :: Double
    tm = fromIntegral tm'
    sm :: Double
    sm = fromIntegral sm'
    c = 0.5 * sqrt (tm * tm - 4 * sm)

intervalLength :: (Int, Int) -> Int
intervalLength (a, b) = b - a + 1

pInput :: Parser ([Int], [Int])
pInput = do
  _ <- string "Time:"
  skipSpace
  ts <- decimal `sepBy1'` skipSpace
  endOfLine
  _ <- string "Distance:"
  skipSpace
  ds <- decimal `sepBy1'` skipSpace
  endOfLine
  endOfInput
  return (ts, ds)

nWays :: Int -> Int -> Int
nWays tm sm = intervalLength $ qu tm sm

solve :: ([Int], [Int]) -> [Int]
solve (xs, ys) = zipWith nWays xs ys

rmSpace :: String -> String
rmSpace [] = []
rmSpace (' ' : xs) = rmSpace xs
rmSpace (x : xs) = x : rmSpace xs

conc :: [Int] -> Int
conc = read . rmSpace . unwords . map show

solve2 :: ([Int], [Int]) -> Int
solve2 (xs, ys) = nWays (conc xs) (conc ys)

main :: IO ()
main = do
  d <- parseChallengeT (Full 6) pInput
  print $ product $ solve d
  print $ solve2 d
