{-# LANGUAGE DeriveGeneric #-}

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
import Aoc.Tuple (sortTuple)
import Data.Attoparsec.Text (Parser, char, decimal, endOfLine, sepBy1')
import Data.Hashable (Hashable (..))
import Data.Set (Set)
import qualified Data.Set as S
import GHC.Generics (Generic)

data Point = Point {xPos :: Int, yPos :: Int, zPos :: Int}
  deriving (Show, Eq, Read, Generic)

instance Ord Point where
  x `compare` y = (zPos x, xPos x, yPos x) `compare` (zPos y, xPos y, yPos y)

instance Hashable Point

data Brick = Brick {from :: Point, to :: Point, hashValue :: Int}
  deriving (Show, Eq, Read)

instance Ord Brick where
  x `compare` y = (to x, from x, hashValue x) `compare` (to y, from y, hashValue y)

pPoint :: Parser Point
pPoint = Point <$> decimal <* char ',' <*> decimal <* char ',' <*> decimal

pBrick :: Parser Brick
pBrick = do
  p1 <- pPoint
  _ <- char '~'
  p2 <- pPoint
  let (f, t) = sortTuple (p2, p1)
  pure $ Brick f t (hash (f, t))

pInput :: Parser (Set Brick)
pInput = S.fromList <$> pBrick `sepBy1'` endOfLine

fallPoint :: Int -> Point -> Point
fallPoint n (Point x y z) = Point x y (z - n)

fallBrick :: Int -> Brick -> Brick
fallBrick n (Brick f t h) = Brick (fallPoint n f) (fallPoint n t) h

overlap :: (Int, Int) -> (Int, Int) -> Bool
overlap (a', b') (c', d') = a <= d && b >= c
  where
    (a, b) = sortTuple (a', b')
    (c, d) = sortTuple (c', d')

crosses :: Brick -> Brick -> Bool
crosses (Brick f1 t1 _) (Brick f2 t2 _) =
  overlap (xPos f1, xPos t1) (xPos f2, xPos t2)
    && overlap (yPos f1, yPos t1) (yPos f2, yPos t2)

fallOnTop :: Set Brick -> Brick -> Set Brick
fallOnTop xs b = let b' = fallBrick (zOfFallingBrick - zOfHighestBrick - 1) b in S.insert b' xs
  where
    mHighestBrick = S.lookupMax $ S.filter (crosses b) xs
    zOfHighestBrick = case mHighestBrick of
      Nothing -> 0
      Just highestBrick -> zPos $ to highestBrick
    zOfFallingBrick = zPos $ from b

fall :: Set Brick -> Set Brick
fall = S.foldl' fallOnTop S.empty

-- -- That's how I solved part one originally.
-- findNonSupporters :: Set Brick -> Set Brick
-- findNonSupporters xs = S.filter predicate xs
--   where
--     predicate x = let xs' = S.delete x xs in xs' == fall xs'

findNumberOfFallers :: Set Brick -> [Int]
findNumberOfFallers xs = map nFallers $ S.toList xs
  where
    nFallers x =
      let xs' = S.delete x xs
          xs'' = fall xs'
       in if fall xs'' /= xs''
            then error ""
            else S.size $ xs'' S.\\ xs'

main :: IO ()
main = do
  bs <- parseChallengeT (Full 22) pInput
  let fallenBs = fall bs
  let fallers = findNumberOfFallers fallenBs
  -- Part 1.
  print $ length $ filter (== 0) fallers
  print $ sum $ findNumberOfFallers fallenBs
