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
import Aoc.Char (readDigit)
import Control.Applicative (asum)
import Data.Attoparsec.Text (Parser, endOfInput, endOfLine, isEndOfLine, sepBy1', takeTill)
import Data.Char (isDigit)
import Data.List (tails)
import Data.Maybe (fromJust)
import qualified Data.Text as T

pLine :: Parser Text
pLine = takeTill isEndOfLine

pInput :: Parser [Text]
pInput = init <$> (pLine `sepBy1'` endOfLine <* endOfInput)

solveLn :: Text -> Int
solveLn = read . (\xs -> [head xs, last xs]) . filter isDigit . T.unpack

pDigit :: String -> Maybe Int
pDigit xs
  | take 3 xs == "one" = Just 1
  | take 3 xs == "two" = Just 2
  | take 5 xs == "three" = Just 3
  | take 4 xs == "four" = Just 4
  | take 4 xs == "five" = Just 5
  | take 3 xs == "six" = Just 6
  | take 5 xs == "seven" = Just 7
  | take 5 xs == "eight" = Just 8
  | take 4 xs == "nine" = Just 9
  | null xs = Nothing
  | isDigit $ head xs = Just $ readDigit $ head xs
  | otherwise = Nothing

getFirst :: String -> Int
getFirst xs = fromJust $ asum $ map pDigit $ tails xs

getLast :: String -> Int
getLast xs = fromJust $ asum $ map pDigit $ reverse $ tails xs

main :: IO ()
main = do
  d <- parseChallengeT (F 1) pInput
  print d
  print $ sum $ map solveLn d
  let xs = map (getFirst . T.unpack) d
      ys = map (getLast . T.unpack) d
  print $ sum $ zipWith (\x y -> (10 * x) + y) xs ys
