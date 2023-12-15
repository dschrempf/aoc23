{-# LANGUAGE RankNTypes #-}

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
import Aoc.Array (filterA, nCols, nRows, pMatrix)
import Data.Attoparsec.Text (Parser, endOfInput, endOfLine, sepBy1', skipSpace)
import Data.List (singleton)
import Data.Massiv.Array (Array, B, D, Dimension (..), Ix2 (..), Source, Sz (..))
import qualified Data.Massiv.Array as A
import Data.Maybe (catMaybes)

data Point = Ash | Rock
  deriving (Eq)

charMap :: Point -> Char
charMap Ash = '.'
charMap Rock = '#'

instance Show Point where
  show = singleton . charMap

type Field = Array B Ix2 Point

pField :: Parser Field
pField = pMatrix [('.', Ash), ('#', Rock)]

pInput :: Parser [Field]
pInput = pField `sepBy1'` skipSpace <* endOfLine <* endOfInput

isHorizontalMirrorWith ::
  (Source r' e) =>
  (Array D Ix2 e -> Array D Ix2 e -> Bool) ->
  Int ->
  Array r' Ix2 e ->
  Bool
isHorizontalMirrorWith f n xs = f (extract (A.reverse Dim2 x)) (extract y)
  where
    (x, y) = A.splitAt' 2 n xs
    minRows = min (nRows x) (nRows y)
    sz = Sz $ minRows :. nCols x
    extract = A.extract' (0 :. 0) sz

findIndexWith :: (Source r e) => (Int -> Array r Ix2 e -> Bool) -> Array r Ix2 e -> Maybe Int
findIndexWith f xs
  | null rs = Nothing
  | otherwise = Just l
  where
    r = nRows xs
    (ls, rs) = break (`f` xs) [1 .. r - 1]
    l = succ $ length ls

findHorizontalMirror :: (Source r e, Eq e) => Array r Ix2 e -> Maybe Int
findHorizontalMirror = findIndexWith (isHorizontalMirrorWith (==))

findVerticalMirror :: (Source r e, Eq e) => Array r Ix2 e -> Maybe Int
findVerticalMirror = findHorizontalMirror . A.transpose

hasOneDifference :: (Eq e, Source r e) => Array r Ix2 e -> Array r Ix2 e -> Bool
hasOneDifference a = (== 1) . length . filterA id . A.zipWith (/=) a

findOtherHorizontalMirror :: (Source r e, Eq e) => Array r Ix2 e -> Maybe Int
findOtherHorizontalMirror = findIndexWith (isHorizontalMirrorWith hasOneDifference)

findOtherVerticalMirror :: (Source r e, Eq e) => Array r Ix2 e -> Maybe Int
findOtherVerticalMirror = findOtherHorizontalMirror . A.transpose

main :: IO ()
main = do
  fs <- parseChallengeT (Full 13) pInput
  let hs1 = map findHorizontalMirror fs
      vs1 = map findVerticalMirror fs
  print hs1
  print vs1
  print $ sum (catMaybes vs1) + 100 * sum (catMaybes hs1)
  let hs2 = map findOtherHorizontalMirror fs
      vs2 = map findOtherVerticalMirror fs
  print hs2
  print vs2
  print $ sum (catMaybes vs2) + 100 * sum (catMaybes hs2)
