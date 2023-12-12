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

import Aoc
import Control.Applicative (asum)
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
import Data.Maybe (fromMaybe)

type Number = Int

type Numbers = [Number]

pSeeds :: Parser Numbers
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

pInput :: Parser (Numbers, [Map])
pInput = do
  s <- pSeeds
  endOfLine
  endOfLine
  ms <- pMap `sepBy1'` (endOfLine <* endOfLine)
  return (s, ms)

mapNumberMapEntry :: Number -> MapEntry -> Maybe Number
mapNumberMapEntry n (MapEntry d s r)
  | delta < 0 = Nothing
  | delta < r = Just $ d + delta
  | otherwise = Nothing
  where
    delta = n - s

mapNumberMap :: Number -> Map -> Number
mapNumberMap n m = fromMaybe n (asum $ map (mapNumberMapEntry n) m)

mapNumber :: [Map] -> Number -> Number
mapNumber ms n = foldl mapNumberMap n ms

getSeeds2 :: Numbers -> Numbers
getSeeds2 = go
  where
    go [] = []
    go (x : y : xs) = [x .. x + y - 1] ++ go xs
    go _ = error "uneven"

main :: IO ()
main = do
  d <- parseChallengeT (Full 5) pInput
  let seeds1 = fst d
      ms = snd d
      f = mapNumber ms
  print $ minimum $ map f seeds1
  let seeds2 = getSeeds2 $ fst d
  print $ minimum $ map f seeds2
