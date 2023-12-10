-- |
-- Module      :  Aoc.Direction
-- Description :  Directions on a 2D grid
-- Copyright   :  2023 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Sun Dec 10 20:37:01 2023.
module Aoc.Direction
  ( Direction (..),
    ix2ToDirections,
    directionToIx2,
  )
where

import Data.Massiv.Array (Ix2 (..))
import Data.Maybe (catMaybes)

data Direction = North | East | South | West
  deriving (Show, Eq, Bounded, Enum)

toDirectionNS :: Int -> Maybe Direction
toDirectionNS x = case compare x 0 of
  GT -> Just South
  LT -> Just North
  EQ -> Nothing

toDirectionEW :: Int -> Maybe Direction
toDirectionEW x = case compare x 0 of
  LT -> Just West
  GT -> Just East
  EQ -> Nothing

-- | Extract direction components.
ix2ToDirections :: Ix2 -> [Direction]
ix2ToDirections (x :. y) = catMaybes [toDirectionNS x, toDirectionEW y]

-- | Get shortest delta index from direction.
directionToIx2 :: Direction -> Ix2
directionToIx2 North = -1 :. 0
directionToIx2 East = 0 :. 1
directionToIx2 South = 1 :. 0
directionToIx2 West = 0 :. -1
