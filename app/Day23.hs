-- |
-- Module      :  Main
-- Description :  Day 23
-- Copyright   :  2023 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Mon Nov  6 05:48:34 2023.
--
-- See https://adventofcode.com/2023/day/23.
module Main
  ( main,
  )
where

import Aoc
import Aoc.Array (neighborsNoDiagonal, parseMatrix)
import Aoc.Direction (Direction (..), moveNStepsInDirection)
import Aoc.Function (fixPoint)
import Control.Monad (foldM)
import Data.Attoparsec.Text (Parser)
import Data.Massiv.Array (Array, B, Ix2 (..), Sz (..))
import qualified Data.Massiv.Array as A
import Data.Maybe (catMaybes, fromJust)
import qualified Data.PartialOrd as P
import Data.Set (Set)
import qualified Data.Set as S

data Tile = PathTile | Forest | Slope !Direction
  deriving (Eq)

instance Show Tile where
  show PathTile = "."
  show Forest = "#"
  show (Slope d) = case d of
    East -> ">"
    South -> "v"
    West -> "<"
    North -> "^"

type Field = Array B Ix2 Tile

pField :: Parser Field
pField =
  parseMatrix
    [ ('#', Forest),
      ('.', PathTile),
      ('>', Slope East),
      ('v', Slope South),
      ('<', Slope West),
      ('^', Slope North)
    ]

findStart :: Field -> Path
findStart f = Path start start S.empty
  where
    start = fromJust $ A.findIndex (== PathTile) f

findEnd :: Field -> Ix2
findEnd f = pred m :. col
  where
    (Sz (m :. _)) = A.size f
    lastRow = f A.!> pred m
    col = fromJust $ A.findIndex (== PathTile) lastRow

data Path = Path
  { curPos :: Ix2,
    lastPost :: Ix2,
    path :: Set Ix2
  }
  deriving (Show, Eq, Ord)

instance P.PartialOrd Path where
  (Path curL lstL xsL) <= (Path curR lstR xsR)
    | curL == curR && lstL == lstR = S.size xsL <= S.size xsR
    | otherwise = False

move :: Ix2 -> Field -> Path -> [Path]
move end f p
  | curPos p == end = [p]
  | otherwise =
      catMaybes
        [moveTo1 f ix p | ix <- neighborsNoDiagonal (A.size f) (curPos p)]

moveSet :: Ix2 -> Field -> Set Path -> Set Path
moveSet end f ps = S.fromList ps'
  where
    ps' = concat [move end f p | p <- S.toList ps]

addTile :: Path -> Ix2 -> Maybe Path
addTile (Path cur lst xs) ix
  | ix == cur = Nothing
  | ix == lst = Nothing
  | ix `S.member` xs = Nothing
  | otherwise = Just $ Path ix cur $ S.insert lst xs

addTiles :: [Ix2] -> Path -> Maybe Path
addTiles xs pth = foldM addTile pth xs

moveTo1 :: Field -> Ix2 -> Path -> Maybe Path
moveTo1 f ix p = case f A.! ix of
  PathTile -> addTile p ix
  Forest -> Nothing
  Slope d -> addTiles [ix, moveNStepsInDirection 1 ix d] p

-- moveSet2 :: Ix2 -> Field -> (Set Path, Set Path) -> (Set Path, Set Path)
-- moveTo2 :: Field -> Ix2 -> Path -> Maybe Path
-- moveTo2 f ix p = case f A.! ix of
--   Forest -> Nothing
--   _other -> addTile p ix

pLength :: Path -> Int
pLength (Path _ _ xs) = 1 + S.size xs

main :: IO ()
main = do
  f <- parseChallengeT (Sample 23 1) pField
  let s = S.singleton $ findStart f
      e = findEnd f
  let ps = fixPoint (moveSet e f) s
  print $ maximum $ S.map pLength ps
