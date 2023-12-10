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
import Aoc.Bounded (predWrap, succWrap)
import Aoc.Direction (Direction, directionToIx2, ix2ToDirections)
import Aoc.Set (flatten)
import Control.Applicative (Alternative (..))
import Control.DeepSeq (force)
import Data.Attoparsec.Text (Parser, char, choice, endOfLine, sepBy1')
import Data.Bifunctor (Bifunctor (..))
import Data.List (nub)
import Data.Massiv.Array (Array, B, Comp (..), Ix2 (..), Sz (..), (!))
import qualified Data.Massiv.Array as A
import qualified Data.Set as S

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

findNumberOfSteps :: Map -> S.Set Ix2 -> Int
findNumberOfSteps m = go 0 S.empty
  where
    go n lasts currents =
      let lasts' = currents
          currents' = force $ step m lasts currents
          !n' = succ n
       in if length currents' <= 1
            then n'
            else go n' lasts' currents'

firstStep :: Map -> Ix2 -> Ix2
firstStep m s = head $ S.toList $ step m S.empty (S.singleton s)

findPipeLoop :: Map -> Ix2 -> [Ix2]
findPipeLoop m s = reverse $ go [s] s (firstStep m s)
  where
    go visitedIxs lastIx currentIx =
      let visitedIxs' = currentIx : visitedIxs
          lastIx' = currentIx
          currentIx' =
            force $
              head $
                S.toList $
                  step m (S.singleton lastIx) (S.singleton currentIx)
       in if currentIx' == s
            then currentIx' : visitedIxs'
            else go visitedIxs' lastIx' currentIx'

getLoopWithDirections :: [Ix2] -> [(Ix2, Direction)]
getLoopWithDirections (x : y : z : xs) = [(y, d) | d <- directions] ++ goOn
  where
    directions = ix2ToDirections $ z - x
    goOn = getLoopWithDirections (y : z : xs)
getLoopWithDirections _ = []

raytraceUntil :: Sz Ix2 -> S.Set Ix2 -> Ix2 -> Direction -> [Ix2]
raytraceUntil sz lp ix dir
  | ix' `S.member` lp || not (A.isSafeIndex sz ix') = []
  | otherwise = ix' : raytraceUntil sz lp ix' dir
  where
    ix' = ix + directionToIx2 dir

ixsRightOf :: Sz Ix2 -> S.Set Ix2 -> Ix2 -> Direction -> [Ix2]
ixsRightOf sz lp ix dir = raytraceUntil sz lp ix (succWrap dir)

ixsLeftOf :: Sz Ix2 -> S.Set Ix2 -> Ix2 -> Direction -> [Ix2]
ixsLeftOf sz lp ix dir = raytraceUntil sz lp ix (predWrap dir)

getRightAndLeftTiles :: Sz Ix2 -> S.Set Ix2 -> [(Ix2, Direction)] -> ([Ix2], [Ix2])
getRightAndLeftTiles sz lp = go [] []
  where
    go lefts rights [] = (lefts, rights)
    go lefts rights ((x, dir) : xs) =
      go
        (ixsLeftOf sz lp x dir ++ lefts)
        (ixsRightOf sz lp x dir ++ rights)
        xs

atBorder :: Sz Ix2 -> Ix2 -> Bool
atBorder (Sz (rows :. cols)) (m :. n) = m == 0 || n == 0 || m == pred rows || n == pred cols

isOutside :: Sz Ix2 -> [Ix2] -> Bool
isOutside sz = any (atBorder sz)

main :: IO ()
main = do
  m <- parseChallengeT (Full 10) pMap
  let s = findStart m
  print $ findNumberOfSteps m $ S.singleton s
  let pipeLoop = findPipeLoop m s
      pipeLoopSet = S.fromList pipeLoop
      loopWithDirections = getLoopWithDirections $ last pipeLoop : pipeLoop
      mapSize = A.size m
      (leftTiles, rightTiles) =
        bimap nub nub $
          getRightAndLeftTiles mapSize pipeLoopSet loopWithDirections
  print $ length $ case (isOutside mapSize leftTiles, isOutside mapSize rightTiles) of
    (True, False) -> rightTiles
    (False, True) -> leftTiles
    _ -> error "could not determine inside"
