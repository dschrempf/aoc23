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
import Data.List (partition)
import Data.Massiv.Array (Array, B, Ix2 (..), Sz (..))
import qualified Data.Massiv.Array as A
import Data.Maybe (catMaybes, fromJust)
import Data.Set (Set)
import qualified Data.Set as S
import Debug.Trace (traceShow, traceShowId)

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

moveTo2 :: Field -> Ix2 -> Path -> Maybe Path
moveTo2 f ix p = case f A.! ix of
  Forest -> Nothing
  _other -> addTile p ix

-- moveLs2 :: Ix2 -> Field -> ([Path], [Path]) -> ([Path], [Path])
-- moveLs2 end f (done, ps) = (done <> done', traceShow (length ps'') ps'')
--   where
--     ps' =
--       catMaybes
--         [ moveTo2 f ix p
--           | p <- ps,
--             ix <- neighborsNoDiagonal (A.size f) (curPos p)
--         ]
--     (done', ps'') = partition ((== end) . curPos) ps'

moveLs2State :: Ix2 -> Field -> (Int, Set Path, [Path]) -> (Int, Set Path, [Path])
moveLs2State end f (i, done, ps) =
  ( succ i,
    S.union done (S.fromList done'),
    traceShow i $ traceShow (length ps''') ps'''
  )
  where
    ps' =
      catMaybes
        [ moveTo2 f ix p
          | p <- ps,
            ix <- neighborsNoDiagonal (A.size f) (curPos p)
        ]
    (done', ps'') = partition ((== end) . curPos) ps'
    ps''' = if i `mod` 1000 == 0 then S.toList $ S.fromList ps'' else ps''

pLength :: Path -> Int
pLength (Path _ _ xs) = 1 + S.size xs

main :: IO ()
main = do
  f <- parseChallengeT (Full 23) pField
  let s = S.singleton $ findStart f
      e = findEnd f
  -- let ps = fixPoint (moveSet e f) s
  -- print $ maximum $ S.map pLength ps
  let (_, ps2, _) = until (\(_, _, xs) -> null xs) (moveLs2State e f) (0, S.empty, [findStart f])
  print $ maximum $ S.map pLength ps2
