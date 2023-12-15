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

import Aoc
import Aoc.Array (filterA, insertCols, insertRows)
import Control.Applicative (Alternative (..))
import Data.Attoparsec.Text (Parser, char, choice, endOfInput, endOfLine, sepBy1')
import Data.Massiv.Array (Array, B, Comp (..), Ix2 (..), Sz (..), (!>), (<!))
import qualified Data.Massiv.Array as A

data Pixel = EmptySpace | Galaxy
  deriving (Eq)

instance Show Pixel where
  show EmptySpace = "."
  show Galaxy = "#"

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
addEmptyRows xs = go 0 xs
  where
    go :: Int -> Image -> [Int] -> Image
    go _ image [] = image
    go offset image (n : ns) =
      let image' = insertRows emptyRow (n + offset) image
       in go (succ offset) image' ns
    (Sz (_ :. nCols)) = A.size xs
    emptyRow = A.replicate Seq (Sz $ 1 :. nCols) EmptySpace

addEmptyCols :: Image -> [Int] -> Image
addEmptyCols xs = go 0 xs
  where
    go :: Int -> Image -> [Int] -> Image
    go _ image [] = image
    go offset image (n : ns) =
      let image' = insertCols emptyCol (n + offset) image
       in go (succ offset) image' ns
    (Sz (nRows :. _)) = A.size xs
    emptyCol = A.replicate Seq (Sz $ nRows :. 1) EmptySpace

findGalaxies :: Image -> [Ix2]
findGalaxies = filterA (== Galaxy)

distance1 :: Ix2 -> Ix2 -> Int
distance1 (mx :. nx) (my :. ny) = abs (my - mx) + abs (ny - nx)

distance1D :: [Int] -> Int -> Int -> Int
distance1D empties x1 x2 = right - left + 999999 * length nEmptiesBetween
  where
    (left, right) = if x1 <= x2 then (x1, x2) else (x2, x1)
    nEmptiesBetween = filter (\e -> e > left && e < right) empties

distance2 :: [Int] -> [Int] -> Ix2 -> Ix2 -> Int
distance2 emptyRows emptyCols (mx :. nx) (my :. ny) =
  distance1D emptyRows mx my + distance1D emptyCols nx ny

main :: IO ()
main = do
  im <- parseChallengeT (Full 11) pImage
  let emptyRows = findEmptyRows im
      emptyCols = findEmptyCols im
      im' = (`addEmptyCols` emptyCols) $ addEmptyRows im emptyRows
      gs1 = findGalaxies im'
      ds1 = [distance1 a b | a <- gs1, b <- gs1, a /= b, a < b]
  print $ sum ds1
  let gs2 = findGalaxies im
      ds2 = [distance2 emptyRows emptyCols a b | a <- gs2, b <- gs2, a /= b, a < b]
  print $ sum ds2
