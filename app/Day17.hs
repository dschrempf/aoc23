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
import Data.Map (Map)
import qualified Data.Map as M
import Data.Massiv.Array (Array, B, Ix2 (..), Sz (..))
import qualified Data.Massiv.Array as A
import Data.Maybe (catMaybes, mapMaybe)
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

type OptimalPaths = Map (Ix2, Direction, Natural) Natural

moveStraightRay :: Field -> Ray -> Maybe Ray
moveStraightRay xs (Ray ix dir nStr hL)
  | nStr >= 10 = Nothing
  | otherwise = do
      dHL <- xs A.!? ix'
      pure $ Ray ix' dir (succ nStr) (hL + dHL)
  where
    ix' = moveNStepsInDirection 1 ix dir

turnRightRay :: Ray -> Ray
turnRightRay (Ray ix dir _ hL) = Ray ix (turnRight dir) 0 hL

turnLeftRay :: Ray -> Ray
turnLeftRay (Ray ix dir _ hL) = Ray ix (turnLeft dir) 0 hL

moveOneStepS :: Field -> Ray -> [Ray]
moveOneStepS xs ray =
  mapMaybe
    (moveStraightRay xs)
    $ catMaybes
      [ Just ray,
        if nStraight ray < 4 then Nothing else Just $ turnRightRay ray,
        if nStraight ray < 4 then Nothing else Just $ turnLeftRay ray
      ]

filterRaysAndUpdateOptimalPaths :: [Ray] -> OptimalPaths -> ([Ray], OptimalPaths)
filterRaysAndUpdateOptimalPaths rs ps = foldl accF ([], ps) rs
  where
    accF (goodRays, optimalPaths) ray@(Ray ix dir nStr hL) =
      case optimalPaths M.!? (ix, dir, nStr) of
        Just optHL | optHL <= hL -> (goodRays, optimalPaths)
        _ -> (ray : goodRays, M.insert (ix, dir, nStr) hL optimalPaths)

moveOneStep :: Field -> [Ray] -> OptimalPaths -> ([Ray], OptimalPaths)
moveOneStep xs rs = filterRaysAndUpdateOptimalPaths rs'
  where
    rs' = concatMap (moveOneStepS xs) rs

initialRays :: [Ray]
initialRays = [rayE, rayS]
  where
    rayE = Ray (0 :. 0) East 0 0
    rayS = Ray (0 :. 0) South 0 0

initialOptimalPaths :: OptimalPaths
initialOptimalPaths =
  M.fromList
    [ ((0 :. 0, East, 0), 0),
      ((0 :. 0, South, 0), 0)
    ]

moveUntilFixpoint :: Field -> [Ray] -> OptimalPaths -> OptimalPaths
moveUntilFixpoint xs rs ps
  | null rs' = ps'
  | otherwise = moveUntilFixpoint xs rs' ps'
  where
    (rs', ps') = moveOneStep xs rs ps

main :: IO ()
main = do
  xs <- parseChallengeT (Full 17) pInput
  let ps = moveUntilFixpoint xs initialRays initialOptimalPaths
      (Sz (m :. n)) = A.size xs
  print $
    minimum $
      catMaybes
        [ M.lookup (pred m :. pred n, d, l) ps
          | d <- [minBound .. maxBound :: Direction],
            l <- [4 .. 10 :: Natural]
        ]
