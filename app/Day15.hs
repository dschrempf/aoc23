{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Main
-- Description :  Day 15
-- Copyright   :  2023 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Mon Nov  6 05:48:34 2023.
--
-- See https://adventofcode.com/2023/day/15.
module Main
  ( main,
  )
where

import Aoc
import Data.Attoparsec.Text (Parser, char, sepBy1', takeWhile1)
import Data.Char (ord)
import qualified Data.Text as T

pInput :: Parser [Text]
pInput = takeWhile1 (`notElem` [',', '\n']) `sepBy1'` char ','

hashChar :: Int -> Char -> Int
hashChar n c = (n + ord c) * 17 `mod` 256

hash :: Text -> Int
hash = T.foldl hashChar 0

main :: IO ()
main = do
  d <- parseChallengeT (Full 15) pInput
  print d
  print $ sum $ map hash d
