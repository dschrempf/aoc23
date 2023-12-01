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
  | null xs = Nothing
  | matchesDigit "one" = Just 1
  | matchesDigit "two" = Just 2
  | matchesDigit "three" = Just 3
  | matchesDigit "four" = Just 4
  | matchesDigit "five" = Just 5
  | matchesDigit "six" = Just 6
  | matchesDigit "seven" = Just 7
  | matchesDigit "eight" = Just 8
  | matchesDigit "nine" = Just 9
  | isDigit x = Just $ readDigit x
  | otherwise = Nothing
  where
    matchesDigit :: String -> Bool
    matchesDigit s = let l = length xs in take l xs == s
    x = head xs

getFirstDigit :: String -> Int
getFirstDigit xs = fromJust $ asum $ map pDigit $ tails xs

getLastDigit :: String -> Int
getLastDigit xs = fromJust $ asum $ map pDigit $ reverse $ tails xs

main :: IO ()
main = do
  d <- parseChallengeT (Full 1) pInput
  print d
  print $ sum $ map solveLn d
  let firstDigits = map (getFirstDigit . T.unpack) d
      lastDigits = map (getLastDigit . T.unpack) d
  print $
    sum $
      zipWith
        (\firstDigit lastDigit -> (10 * firstDigit) + lastDigit)
        firstDigits
        lastDigits
