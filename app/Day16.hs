-- |
-- Module      :  Main
-- Description :  Day 16
-- Copyright   :  2023 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Mon Nov  6 05:48:34 2023.
--
-- See https://adventofcode.com/2023/day/16.
module Main
  ( main,
  )
where

import Aoc
import Aoc.Array (pMatrix)
import Aoc.Direction
import Data.Attoparsec.Text (Parser)
import Data.Foldable (Foldable (..), maximumBy)
import Data.Function (on)
import Data.Massiv.Array (Array, B (..), Ix2 (..), Sz (..))
import qualified Data.Massiv.Array as A
import Data.Set (Set)
import qualified Data.Set as S

data Tile = Empty | MirrorSlash | MirrorBackSlash | SplitterV | SplitterH

instance Show Tile where
  show Empty = "."
  show MirrorSlash = "/"
  show MirrorBackSlash = "\\"
  show SplitterV = "|"
  show SplitterH = "-"

type Contraption = Array B Ix2 Tile

pContraption :: Parser Contraption
pContraption =
  pMatrix
    [ ('.', Empty),
      ('/', MirrorSlash),
      ('\\', MirrorBackSlash),
      ('|', SplitterV),
      ('-', SplitterH)
    ]

data Ray = Ray {position :: Ix2, direction :: Direction}
  deriving (Show, Eq, Ord)

type Rays = Set Ray

mirrorSlash :: Direction -> Direction
mirrorSlash North = East
mirrorSlash East = North
mirrorSlash South = West
mirrorSlash West = South

mirrorBackSlash :: Direction -> Direction
mirrorBackSlash North = West
mirrorBackSlash West = North
mirrorBackSlash East = South
mirrorBackSlash South = East

splitV :: Ray -> Rays
splitV (Ray ix dir)
  | isHorizontal dir = S.fromList [Ray ix $ turnRight dir, Ray ix $ turnLeft dir]
  | otherwise = S.singleton $ Ray ix dir

splitH :: Ray -> Rays
splitH (Ray ix dir)
  | isVertical dir = S.fromList [Ray ix $ turnRight dir, Ray ix $ turnLeft dir]
  | otherwise = S.singleton $ Ray ix dir

traceOneStep :: Contraption -> Ray -> Rays
traceOneStep xs (Ray ix dir) = case tile of
  Nothing -> S.empty
  Just Empty -> S.singleton $ Ray ix' dir
  Just MirrorSlash -> S.singleton $ Ray ix' $ mirrorSlash dir
  Just MirrorBackSlash -> S.singleton $ Ray ix' $ mirrorBackSlash dir
  Just SplitterV -> splitV (Ray ix' dir)
  Just SplitterH -> splitH (Ray ix' dir)
  where
    ix' = moveNStepsInDirection 1 ix dir
    tile = xs A.!? ix'

type VisitedTiles = Rays

next :: Contraption -> Rays -> VisitedTiles -> (Rays, VisitedTiles)
next xs rays visited = (uniqueNewRays, visited `S.union` uniqueNewRays)
  where
    newRays = fold $ S.map (traceOneStep xs) rays
    uniqueNewRays = newRays S.\\ visited

traceWith :: Ray -> Contraption -> VisitedTiles
traceWith ray xs = go firstRay firstRay xs
  where
    firstRay = traceOneStep xs ray
    go rays visited ys
      | visited == newVisited = visited
      | otherwise = go newRays newVisited ys
      where
        (newRays, newVisited) = next ys rays visited

nTilesWith :: Ray -> Contraption -> Int
nTilesWith ray = length . S.map position . traceWith ray

startingRays :: Contraption -> [Ray]
startingRays xs =
  [Ray (-1 :. n) South | n <- [0 .. pred nMax]]
    ++ [Ray (mMax :. n) North | n <- [0 .. pred nMax]]
    ++ [Ray (m :. -1) East | m <- [0 .. pred mMax]]
    ++ [Ray (m :. nMax) West | m <- [0 .. pred mMax]]
  where
    (Sz (mMax :. nMax)) = A.size xs

main :: IO ()
main = do
  d <- parseChallengeT (Full 16) pContraption
  print $ nTilesWith (Ray (0 :. -1) East) d
  print $ maximumBy (compare `on` fst) [(nTilesWith ray d, ray) | ray <- startingRays d]
