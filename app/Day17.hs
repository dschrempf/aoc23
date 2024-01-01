-- |
-- Module      :  Main
-- Description :  Day 17
-- Copyright   :  2023 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Mon Nov  6 05:48:34 2023.
--
-- See https://adventofcode.com/2023/day/17.
module Main
  ( main,
  )
where

import Aoc
import Aoc.Array (pMatrix)
import Aoc.Direction (Direction (..), moveNStepsInDirection, turnLeft, turnRight)
import Data.Attoparsec.Text (Parser)
import Data.List (sort)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Massiv.Array (Array, B, Ix2 (..))
import qualified Data.Massiv.Array as A
import Data.Maybe (mapMaybe)
import Data.PartialOrd (PartialOrd)
import qualified Data.PartialOrd as P
import Debug.Trace
import Numeric.Natural (Natural)

type Field = Array B Ix2 Natural

pInput :: Parser Field
pInput =
  pMatrix
    [ ('1', 1),
      ('2', 2),
      ('3', 3),
      ('4', 4),
      ('5', 5),
      ('6', 6),
      ('7', 7),
      ('8', 8),
      ('9', 9)
    ]

data Ray = Ray
  { position :: Ix2,
    direction :: Direction,
    nStraight :: Natural,
    heatLoss :: Natural
  }
  deriving (Show, Eq, Ord)

instance PartialOrd Ray where
  x <= y =
    (position x == position y)
      && (direction x == direction y)
      && if nStraight x == nStraight y
        then heatLoss x <= heatLoss y
        else nStraight x <= nStraight y && heatLoss x <= heatLoss y

instance PartialOrd RayPath where
  rayPathX <= rayPathY =
    let x = rayHead rayPathX
        yH = rayHead rayPathY
     in x P.<= yH || case M.lookup (position x) (rayPath rayPathY) of
          Nothing -> False
          Just y -> x P.<= y

data RayPath = RayPath
  { rayHead :: Ray,
    rayPath :: Map Ix2 Ray,
    endOfLine :: Bool
  }
  deriving (Show, Eq, Ord)

moveStraightRay :: Field -> Ray -> Maybe Ray
moveStraightRay xs (Ray ix dir nStr hL) = do
  dHL <- xs A.!? ix'
  pure $ Ray ix' dir (succ nStr) (hL + dHL)
  where
    ix' = moveNStepsInDirection 1 ix dir

moveStraightRayPath :: Field -> RayPath -> Maybe RayPath
moveStraightRayPath xs (RayPath rHead rPath eol) = case moveStraightRay xs rHead of
  Nothing -> Just $ RayPath rHead rPath True
  Just rHead' ->
    if nStraight rHead' > 3
      then Nothing
      else Just $ RayPath rHead' (M.insert (position rHead') rHead' rPath) eol

turnRightRay :: Ray -> Ray
turnRightRay (Ray ix dir _ hL) = Ray ix (turnRight dir) 0 hL

turnLeftRay :: Ray -> Ray
turnLeftRay (Ray ix dir _ hL) = Ray ix (turnLeft dir) 0 hL

moveOneStepS :: Field -> RayPath -> [RayPath]
moveOneStepS xs rp@(RayPath rHead rTail eol)
  | eol = [rp]
  | otherwise =
      mapMaybe
        (moveStraightRayPath xs)
        [ RayPath rHead rTail eol,
          RayPath (turnRightRay rHead) rTail eol,
          RayPath (turnLeftRay rHead) rTail eol
        ]

moveOneStep :: Field -> [RayPath] -> [RayPath]
moveOneStep xs rs = P.minima $ concatMap (moveOneStepS xs) rs

firstRayPaths :: [RayPath]
firstRayPaths =
  [ RayPath rayE (M.singleton (0 :. 0) rayE) False,
    RayPath rayS (M.singleton (0 :. 0) rayS) False
  ]
  where
    rayE = Ray (0 :. 0) East 0 0
    rayS = Ray (0 :. 0) South 0 0

moveUntilFixpoint :: Field -> [RayPath] -> [RayPath]
moveUntilFixpoint xs rs
  | rs == rs' = rs
  | otherwise = moveUntilFixpoint xs (traceShow (length $ filter (not . endOfLine) rs') rs')
  where
    rs' = sort $ moveOneStep xs rs

main :: IO ()
main = do
  xs <- parseChallengeT (Sample 17 1) pInput
  print $ moveUntilFixpoint xs firstRayPaths
