-- |
-- Module      :  Main
-- Description :  Day 22
-- Copyright   :  2023 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Mon Nov  6 05:48:34 2023.
--
-- See https://adventofcode.com/2023/day/22.
module Main
  ( main,
  )
where

import Aoc
import Data.Attoparsec.Text (Parser, char, decimal, endOfLine, sepBy1')
import Data.Set (Set)
import qualified Data.Set as S

data Point = Point {xPos :: Int, yPos :: Int, zPos :: Int}
  deriving (Show, Eq, Ord)

data Brick = Brick {from :: Point, to :: Point}
  deriving (Show, Eq, Ord)

pPoint :: Parser Point
pPoint = Point <$> decimal <* char ',' <*> decimal <* char ',' <*> decimal

sortTuple :: (Ord a) => (a, a) -> (a, a)
sortTuple (a, b)
  | a <= b = (a, b)
  | otherwise = (b, a)

pBrick :: Parser Brick
pBrick = do
  p1 <- pPoint
  _ <- char '~'
  p2 <- pPoint
  let (f, t) = sortTuple (p2, p1)
  pure $ Brick f t

pInput :: Parser (Set Brick)
pInput = S.fromList <$> pBrick `sepBy1'` endOfLine

fallPoint :: Int -> Point -> Point
fallPoint n (Point x y z) = Point x y (z - n)

fallBrick :: Int -> Brick -> Brick
fallBrick n (Brick f t) = Brick (fallPoint n f) (fallPoint n t)

overlap :: (Int, Int) -> (Int, Int) -> Bool
overlap (a', b') (c', d') = (a <= c && c <= b) || (d >= a && d <= b)
  where
    (a, b) = sortTuple (a', b')
    (c, d) = sortTuple (c', d')

crosses :: Brick -> Brick -> Bool
crosses (Brick f1 t1) (Brick f2 t2) =
  overlap (xPos f1, xPos t1) (xPos f2, xPos t2)
    && overlap (yPos f1, yPos t1) (yPos f2, yPos t2)

main :: IO ()
main = do
  bs <- parseChallengeT (Sample 22 1) pInput
  mapM_ print [(b1, b2) | b1 <- S.toList bs, b2 <- S.toList bs, b1 /= b2, crosses b1 b2]
