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

move :: Ix2 -> Field -> Path -> [Path]
move end f p
  | curPos p == end = [p]
  | otherwise =
      catMaybes
        [moveTo f ix p | ix <- neighborsNoDiagonal (A.size f) (curPos p)]

moveSet :: Ix2 -> Field -> Set Path -> Set Path
moveSet end f ps = S.fromList $ concat [move end f p | p <- S.toList ps]

addTile :: Path -> Ix2 -> Maybe Path
addTile (Path cur lst xs) ix
  | ix == cur = Nothing
  | ix == lst = Nothing
  | ix `S.member` xs = Nothing
  | otherwise = Just $ Path ix cur $ S.insert lst xs

addTiles :: [Ix2] -> Path -> Maybe Path
addTiles xs pth = foldM addTile pth xs

moveTo :: Field -> Ix2 -> Path -> Maybe Path
moveTo f ix p = case f A.! ix of
  Forest -> Nothing
  _other -> addTile p ix

-- PathTile -> addTile p ix
-- Forest -> Nothing
-- Slope d -> addTiles [ix, moveNStepsInDirection 1 ix d] p

pLength :: Path -> Int
pLength (Path _ _ xs) = 1 + S.size xs

main :: IO ()
main = do
  f <- parseChallengeT (Full 23) pField
  let s = S.singleton $ findStart f
      e = findEnd f
  let ps = fixPoint (moveSet e f) s
  mapM_ print ps
  mapM_ print $ S.map pLength ps
