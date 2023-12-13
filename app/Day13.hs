-- |
-- Module      :  Main
-- Description :  Day 13
-- Copyright   :  2023 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Mon Nov  6 05:48:34 2023.
--
-- See https://adventofcode.com/2023/day/13.
module Main
  ( main,
  )
where

import Aoc (Challenge (..), parseChallengeT)
import Aoc.Array (nCols, nRows, pMatrix)
import Data.Attoparsec.Text (Parser, endOfInput, endOfLine, sepBy1', skipSpace)
import Data.Massiv.Array (Array, B, Dimension (..), Ix2 (..), Size, Source, Sz (..))
import qualified Data.Massiv.Array as A
import Data.Maybe (catMaybes)
import Debug.Trace (traceShow, traceShowId)

data Point = Ash | Rock
  deriving (Eq)

instance Show Point where
  show Ash = "."
  show Rock = "#"

type Field = Array B Ix2 Point

pField :: Parser Field
pField = pMatrix [('.', Ash), ('#', Rock)]

pInput :: Parser [Field]
pInput = pField `sepBy1'` skipSpace <* endOfLine <* endOfInput

isHorizontalMirror :: (Source r e, Eq e) => Int -> Array r Ix2 e -> Bool
isHorizontalMirror n xs = extract (A.reverse Dim2 x) == extract y
  where
    (x, y) = A.splitAt' 2 n xs
    minRows = min (nRows x) (nRows y)
    sz = Sz $ minRows :. nCols x
    extract = A.extract' (0 :. 0) sz

findHorizontalMirror :: (Source r e, Eq e) => Array r Ix2 e -> Maybe Int
findHorizontalMirror xs
  | null rs = Nothing
  | otherwise = Just l
  where
    r = nRows xs
    (ls, rs) = break (`isHorizontalMirror` xs) [1 .. r - 1]
    l = succ $ length ls

findVerticalMirror :: (Source r e, Eq e) => Array r Ix2 e -> Maybe Int
findVerticalMirror = findHorizontalMirror . A.transpose

main :: IO ()
main = do
  fs <- parseChallengeT (Full 13) pInput
  let hs = map findHorizontalMirror fs
      vs = map findVerticalMirror fs
  print hs
  print vs
  print $ sum (catMaybes vs) + 100 * sum (catMaybes hs)
