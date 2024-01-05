{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Main
-- Description :  Day 18
-- Copyright   :  2023 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Mon Nov  6 05:48:34 2023.
--
-- See https://adventofcode.com/2023/day/18.
module Main
  ( main,
  )
where

import Aoc
import Aoc.Bounded (predWrap, succWrap)
import Aoc.Direction
  ( Direction (..),
    directionToIx2,
    ix2ToDirections,
    moveNStepsInDirection,
  )
import Aoc.Parse (skipHorizontalSpace)
import Data.Attoparsec.Text
  ( Parser,
    anyChar,
    char,
    choice,
    count,
    decimal,
    endOfInput,
    endOfLine,
    isEndOfLine,
    satisfy,
    sepBy1',
    skipWhile,
    string,
  )
import Data.Bifunctor (Bifunctor (..))
import Data.Char (isHexDigit)
import Data.List (nub, sort, sortOn)
import Data.Massiv.Array (Ix2 (..), Sz (..))
import qualified Data.Massiv.Array as A
import Data.Set (Set)
import qualified Data.Set as S
import Debug.Trace
import GHC.Natural (Natural)

data Instruction = Instruction {direction :: Direction, edgeLength :: Natural}
  deriving (Eq, Show)

pDirectionChar :: Parser Direction
pDirectionChar =
  choice
    [ North <$ char 'U',
      East <$ char 'R',
      South <$ char 'D',
      West <$ char 'L'
    ]

pInstruction :: Parser Instruction
pInstruction = do
  d <- pDirectionChar
  _ <- skipHorizontalSpace
  l <- decimal
  _ <- skipHorizontalSpace
  _ <- skipWhile (not . isEndOfLine)
  pure $ Instruction d l

pDigPlan :: Parser [Instruction]
pDigPlan = pInstruction `sepBy1'` endOfLine <* endOfLine <* endOfInput

type Trench = [Ix2]

excavate :: [Instruction] -> Trench
excavate = snd . foldl accF (0 :. 0, [])
  where
    accF (pos, trench) (Instruction dir len) =
      let poss = getPositions pos dir len
          trench' = trench ++ poss
       in (last poss, trench')
    getPositions pos dir len =
      [ moveNStepsInDirection (fromIntegral n) pos dir
        | n <- [1 .. len]
      ]

getDimensions :: Trench -> (Ix2, Ix2)
getDimensions xs = (topLeft, bottomRight)
  where
    ms = map (fst . A.fromIx2) xs
    ns = map (snd . A.fromIx2) xs
    topLeft = minimum ms :. minimum ns
    bottomRight = maximum ms :. maximum ns

getLoopWithDirections :: [Ix2] -> [(Ix2, Direction)]
getLoopWithDirections (x : y : z : xs) = [(y, d) | d <- directions] ++ goOn
  where
    directions = ix2ToDirections $ z - x
    goOn = getLoopWithDirections (y : z : xs)
getLoopWithDirections _ = []

raytraceUntil :: Sz Ix2 -> Set Ix2 -> Ix2 -> Direction -> [Ix2]
raytraceUntil sz lp ix dir
  | ix' `S.member` lp || not (A.isSafeIndex sz ix') = []
  | otherwise = ix' : raytraceUntil sz lp ix' dir
  where
    ix' = ix + directionToIx2 dir

ixsLeftOf :: Sz Ix2 -> Set Ix2 -> Ix2 -> Direction -> [Ix2]
ixsLeftOf sz lp ix dir = raytraceUntil sz lp ix (predWrap dir)

ixsRightOf :: Sz Ix2 -> Set Ix2 -> Ix2 -> Direction -> [Ix2]
ixsRightOf sz lp ix dir = raytraceUntil sz lp ix (succWrap dir)

getRightAndLeftTiles :: Sz Ix2 -> Set Ix2 -> [(Ix2, Direction)] -> ([Ix2], [Ix2])
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

getSize :: Trench -> Int
getSize trenchShifted =
  let (topLeftShifted, _) = getDimensions trenchShifted
      trench = map (\k -> k - topLeftShifted) trenchShifted
      (_, bottomRight) = getDimensions trench
      trenchSize = Sz $ bottomRight + (1 :. 1)
      trenchPathLoop = last trench : trench
      trenchPathLoopWithDirection = getLoopWithDirections trenchPathLoop
      (leftTiles, rightTiles) =
        bimap nub nub $
          getRightAndLeftTiles trenchSize (S.fromList trenchPathLoop) trenchPathLoopWithDirection
      inside = length $ case (isOutside trenchSize leftTiles, isOutside trenchSize rightTiles) of
        (True, False) -> rightTiles
        (False, True) -> leftTiles
        _ -> error "could not determine inside"
   in inside + length trench

pDirectionInt :: Parser Direction
pDirectionInt =
  choice
    [ North <$ char '3',
      East <$ char '0',
      South <$ char '1',
      West <$ char '2'
    ]

pColorInstruction :: Parser Instruction
pColorInstruction = do
  _ <- anyChar
  _ <- skipHorizontalSpace
  _ <- decimal :: Parser Int
  _ <- skipHorizontalSpace
  _ <- string "(#"
  lS <- count 5 (satisfy isHexDigit)
  d <- pDirectionInt
  _ <- char ')'
  let l = read $ "0x" <> lS
  pure $ Instruction d l

pDigPlanColor :: Parser [Instruction]
pDigPlanColor = pColorInstruction `sepBy1'` endOfLine <* endOfLine <* endOfInput

excavate2 :: [Instruction] -> Trench
excavate2 = foldl accF [0 :. 0]
  where
    accF (pos : trench) (Instruction dir len) =
      let pos' = moveNStepsInDirection (fromIntegral len) pos dir
          trench' = pos' : pos : trench
       in trench'
    accF [] _ = error "no current position"

slices :: Int -> Int -> Int -> Bool
slices x a b = (a < x && x < b) || (a > x && x > b)

-- -- Assume ms are sorted.
-- sliceTrenchH :: [Int] -> Trench -> Trench
-- sliceTrenchH ms ((m1 :. n1) : (m2 :. n2) : xs) =
--   (m1 :. n1) : additionalPointsCorrectOrder ++ sliceTrenchH ms ((m2 :. n2) : xs)
--   where
--     msSlicing = filter (\m -> slices m m1 m2) ms
--     additionalPoints = [m :. n1 | m <- msSlicing]
--     additionalPointsCorrectOrder =
--       if m1 < m2
--         then additionalPoints
--         else reverse additionalPoints
-- sliceTrenchH _ xs = xs

data Area = Inside Int | Outside

type VerticalLine = (Ix2, Ix2)

getVerticalLines :: Trench -> [VerticalLine]
getVerticalLines (x : y : zs)
  | A.lastDim x == A.lastDim y = (x, y) : getVerticalLines (y : zs)
  | otherwise = getVerticalLines (y : zs)
getVerticalLines _ = []

sort2 :: Int -> Int -> (Int, Int)
sort2 a b
  | a <= b = (a, b)
  | otherwise = (b, a)

coversVerticalLine :: Int -> Int -> VerticalLine -> Bool
coversVerticalLine a b (c :. _, d :. _) = c' <= a' && b' <= d'
  where
    (a', b') = sort2 a b
    (c', d') = sort2 c d

getColOfVerticalLine :: VerticalLine -> Int
getColOfVerticalLine (a, _) = A.lastDim a

-- Assume xs are sorted by columns.
areaOfSlice :: Int -> Int -> [VerticalLine] -> Int
areaOfSlice from to xs = traceShow from $ traceShow to $ go Outside linesCoveringAToB
  where
    linesCoveringAToB = traceShowId $ filter (coversVerticalLine from to) xs
    go area (l : ls) = case area of
      (Inside n) -> (getColOfVerticalLine l - n + 1) * (to - from + 1) + go Outside ls
      Outside -> go (Inside $ getColOfVerticalLine l) ls
    go (Inside _) [] = error "areaOfSlice: inside but no more points"
    go _ _ = 0

getSliceIntervals :: [Int] -> [(Int, Int)]
getSliceIntervals (a : b : ms)
  | b - a > 1 =
      (a, a)
        : (succ a, pred b)
        : getSliceIntervals (b : ms)
  | b - a == 1 = (a, a) : getSliceIntervals (b : ms)
  | otherwise = error "getSliceIntervals: forgot nub"
getSliceIntervals [a] = [(a, a)]
getSliceIntervals [] = []

main :: IO ()
main = do
  is1 <- parseChallengeT (Sample 18 1) pDigPlan
  let trenchShifted = excavate is1
  print $ getSize trenchShifted
  -- is2 <- parseChallengeT (Sample 18 1) pDigPlanColor
  let trenchCorners = reverse $ excavate2 is1
  let ms = nub $ sort $ map A.headDim trenchCorners
      verticalLines = getVerticalLines trenchCorners
      sortedVerticalLines = nub $ sortOn (A.lastDim . fst) verticalLines
      sliceIntervals = getSliceIntervals ms
      areas = [areaOfSlice a b sortedVerticalLines | (a, b) <- sliceIntervals]
  print sliceIntervals
  print areas
