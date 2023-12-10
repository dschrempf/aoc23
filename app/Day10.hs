{-# LANGUAGE BangPatterns #-}

-- |
-- Module      :  Main
-- Description :  Day 10
-- Copyright   :  2023 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Mon Nov  6 05:48:34 2023.
--
-- See https://adventofcode.com/2023/day/10.
module Main
  ( main,
  )
where

import Aoc (Challenge (..), parseChallengeT)
import Aoc.Array (filterA, neighborsNoDiagonal)
import Aoc.Set (flatten)
import Control.Applicative (Alternative (..))
import Control.DeepSeq (force)
import Data.Attoparsec.Text (Parser, char, choice, endOfLine, sepBy1')
import Data.Massiv.Array (Array, B, Comp (..), Ix2 (..), (!))
import qualified Data.Massiv.Array as A
import qualified Data.Set as S
import Debug.Trace (traceShowId)

data Tile
  = Ground
  | Start
  | Vertical
  | Horizontal
  | NorthEast
  | NorthWest
  | SouthEast
  | SouthWest
  deriving (Show, Eq)

pPipe :: Parser Tile
pPipe = choice [v, h, ne, nw, se, sw, g, s]
  where
    v = Vertical <$ char '|'
    h = Horizontal <$ char '-'
    ne = NorthEast <$ char 'L'
    nw = NorthWest <$ char 'J'
    se = SouthEast <$ char 'F'
    sw = SouthWest <$ char '7'
    g = Ground <$ char '.'
    s = Start <$ char 'S'

type Map = Array B Ix2 Tile

pMap :: Parser Map
pMap = A.fromLists' Seq <$> some pPipe `sepBy1'` endOfLine

findStart :: Map -> Ix2
findStart = head . filterA (== Start)

connectsTo :: Map -> Ix2 -> S.Set Ix2
connectsTo m ix = S.filter (A.isSafeIndex $ A.size m) $ S.fromList $ case m ! ix of
  Ground -> []
  Start -> neighborsNoDiagonal (A.size m) ix
  Vertical -> [ix + northSouth, ix - northSouth]
  Horizontal -> [ix + eastWest, ix - eastWest]
  NorthEast -> [ix - northSouth, ix + eastWest]
  NorthWest -> [ix - northSouth, ix - eastWest]
  SouthEast -> [ix + northSouth, ix + eastWest]
  SouthWest -> [ix + northSouth, ix - eastWest]
  where
    eastWest = 0 :. 1
    northSouth = 1 :. 0

isConnected :: Map -> Ix2 -> Ix2 -> Bool
isConnected m ix1 ix2 = ix2 `elem` connectsTo m ix1 && ix1 `elem` connectsTo m ix2

step :: Map -> S.Set Ix2 -> S.Set Ix2 -> S.Set Ix2
step m ls = flatten . S.map (dontStepBack . stepSingle)
  where
    stepSingle x = S.filter (isConnected m x) $ connectsTo m x
    dontStepBack = S.filter (`notElem` ls)

stepN :: Map -> S.Set Ix2 -> (S.Set Ix2, Int)
stepN m = go 0 S.empty S.empty
  where
    go n visited lasts currents =
      let visited' = S.union visited currents
          lasts' = currents
          currents' = force $ step m lasts currents
          !n' = succ n
       in if length currents' <= 1
            then let allVisited = S.union visited' currents' in (allVisited, n')
            else go n' visited' lasts' currents'

main :: IO ()
main = do
  m <- parseChallengeT (Sample 10 13) pMap
  let s = findStart m
      (loop, n) = stepN m $ S.singleton s
  print loop
  print n
