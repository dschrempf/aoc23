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
import Control.Applicative (Alternative (..))
import Control.DeepSeq (force)
import Data.Attoparsec.Text (Parser, char, choice, endOfLine, sepBy1')
import Data.List
import Data.Massiv.Array (Array, B, Comp (..), Ix2 (..), (!))
import qualified Data.Massiv.Array as A
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

connectsTo :: Map -> Ix2 -> [Ix2]
connectsTo m ix = filter (A.isSafeIndex $ A.size m) $ case m ! ix of
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

step :: Map -> [Ix2] -> [Ix2] -> [Ix2]
step m ls = nub . concatMap (dontStepBack . stepSingle)
  where
    stepSingle x = filter (isConnected m x) $ connectsTo m x
    dontStepBack = filter (`notElem` ls)

stepN :: Map -> [Ix2] -> Int
stepN m = go 0 []
  where
    go n ls xs =
      let xs' = traceShowId $ force $ step m ls xs
          !n' = traceShowId $ succ n
       in if length xs' <= 1
            then n'
            else go n' xs xs'

main :: IO ()
main = do
  m <- parseChallengeT (Sample 10 13) pMap
  let s = findStart m
  print m
  print s
  print $ stepN m [s]
