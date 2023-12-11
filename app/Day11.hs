-- |
-- Module      :  Main
-- Description :  Day 11
-- Copyright   :  2023 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Mon Nov  6 05:48:34 2023.
--
-- See https://adventofcode.com/2023/day/11.
module Main
  ( main,
  )
where

import Aoc (Challenge (..), parseChallengeT)
import Aoc.Array (filterA)
import Control.Applicative (Alternative (..))
import Data.Attoparsec.Text (Parser, char, choice, endOfInput, endOfLine, sepBy1')
import Data.Massiv.Array (Array, B, Comp (..), Ix2 (..), Sz (..), (!>), (<!))
import qualified Data.Massiv.Array as A

data Pixel = EmptySpace | Galaxy
  deriving (Show, Eq)

pPixel :: Parser Pixel
pPixel = choice [e, g]
  where
    e = EmptySpace <$ char '.'
    g = Galaxy <$ char '#'

type Image = Array B Ix2 Pixel

pImage :: Parser Image
pImage =
  A.fromLists' Seq
    <$> some pPixel `sepBy1'` endOfLine
    <* endOfLine
    <* endOfInput

findEmptyRows :: Image -> [Int]
findEmptyRows xs =
  [ m
    | m <- [0 .. pred nRows],
      null $ filterA (== Galaxy) $ xs !> m
  ]
  where
    (Sz (nRows :. _)) = A.size xs

findEmptyCols :: Image -> [Int]
findEmptyCols xs =
  [ n
    | n <- [0 .. pred nCols],
      null $ filterA (== Galaxy) $ xs <! n
  ]
  where
    (Sz (_ :. nCols)) = A.size xs

addEmptyRows :: Image -> [Int] -> Image
addEmptyRows = undefined

addEmptyCols :: Image -> [Int] -> Image
addEmptyCols = undefined

main :: IO ()
main = do
  im <- parseChallengeT (Sample 11 1) pImage
  print im
  print $ findEmptyRows im
  print $ findEmptyCols im
