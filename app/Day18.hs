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
import Aoc.Direction
  ( Direction (..),
    ix2ToDirections,
    moveNStepsInDirection,
    turnLeft,
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
import Data.Char (isHexDigit)
import Data.List (nub, sort, sortOn)
import Data.Massiv.Array (Ix2 (..))
import qualified Data.Massiv.Array as A
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

excavate :: [Instruction] -> Trench
excavate = reverse . foldl accF [0 :. 0]
  where
    accF (pos : trench) (Instruction dir len) =
      let pos' = moveNStepsInDirection (fromIntegral len) pos dir
          trench' = pos' : pos : trench
       in trench'
    accF [] _ = error "no current position"

data Area = Inside Int | Outside

type VerticalLine = (Ix2, Ix2)

data Turn = TurnLeft | TurnRight
  deriving (Show, Eq)

turnOutside :: Turn
turnOutside = TurnLeft

getVerticalLinesWithTurns :: [(Ix2, Turn)] -> [VerticalLine]
getVerticalLinesWithTurns ((xm :. xn, tX) : y@(ym :. yn, tY) : zs)
  | xn == yn =
      let xm' = if tX == turnOutside then (if xm < ym then succ xm else pred xm) else xm
          ym' = if tY == turnOutside then (if xm < ym then pred ym else succ ym) else ym
       in (xm' :. xn, ym' :. yn) : getVerticalLinesWithTurns (y : zs)
  | otherwise = getVerticalLinesWithTurns (y : zs)
getVerticalLinesWithTurns _ = []

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
areaOfSlice from to xs = go Outside linesCoveringAToB
  where
    linesCoveringAToB = filter (coversVerticalLine from to) xs
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

directionsToTurn :: Direction -> Direction -> Turn
directionsToTurn a b
  | turnLeft a == b = TurnLeft
  | otherwise = TurnRight

getTrenchWithTurns' :: [Ix2] -> [(Ix2, Turn)]
getTrenchWithTurns' (x : y : z : zs) =
  (y, directionsToTurn a b) : getTrenchWithTurns' (y : z : zs)
  where
    a = head $ ix2ToDirections (y - x)
    b = head $ ix2ToDirections (z - y)
getTrenchWithTurns' _ = []

getTrenchWithTurns :: [Ix2] -> [(Ix2, Turn)]
getTrenchWithTurns xs = getTrenchWithTurns' $ before : xs ++ [after]
  where
    before = last (init xs)
    after = head $ tail xs

getArea :: Trench -> Int
getArea trenchCorners = sum areas
  where
    trenchCornersWithTurns = getTrenchWithTurns trenchCorners
    verticalLines = getVerticalLinesWithTurns trenchCornersWithTurns
    sortedVerticalLines = nub $ sortOn (A.lastDim . fst) verticalLines
    ms = nub $ sort $ map A.headDim trenchCorners
    sliceIntervals = getSliceIntervals ms
    areas = [areaOfSlice a b sortedVerticalLines | (a, b) <- sliceIntervals]

main :: IO ()
main = do
  is1 <- parseChallengeT (Full 18) pDigPlan
  let trenchCorners1 = excavate is1
  print $ getArea trenchCorners1
  is2 <- parseChallengeT (Full 18) pDigPlanColor
  let trenchCorners2 = excavate is2
  print $ getArea trenchCorners2
