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
import Aoc.Direction (Direction (..), directionToIx2, ix2ToDirections, moveNStepsInDirection)
import Aoc.Parse (skipHorizontalSpace)
import Data.Attoparsec.Text
  ( Parser,
    char,
    choice,
    decimal,
    endOfInput,
    endOfLine,
    hexadecimal,
    sepBy1',
    string,
  )
import Data.Bifunctor (Bifunctor (..))
import Data.List (nub)
import Data.Massiv.Array (Ix2 (..), Sz (..))
import qualified Data.Massiv.Array as A
import Data.Set (Set)
import qualified Data.Set as S
import Data.Word (Word8)
import GHC.Natural (Natural)

type RGB = (Word8, Word8, Word8)

data Instruction = Instruction {direction :: Direction, edgeLength :: Natural, rgb :: RGB}
  deriving (Eq, Show)

pDirection :: Parser Direction
pDirection =
  choice
    [ North <$ char 'U',
      East <$ char 'R',
      South <$ char 'D',
      West <$ char 'L'
    ]

pColor :: Parser RGB
pColor = do
  _ <- string "(#"
  c <- hexadecimal :: Parser Int
  _ <- char ')'
  let b = c `mod` 256
      g = ((c - b) `quot` 256) `mod` 256
      r = ((c - b) `quot` (256 * 256)) `mod` 256
  pure (fromIntegral r, fromIntegral g, fromIntegral b)

pInstruction :: Parser Instruction
pInstruction = do
  d <- pDirection
  _ <- skipHorizontalSpace
  l <- decimal
  _ <- skipHorizontalSpace
  Instruction d l <$> pColor

pDigPlan :: Parser [Instruction]
pDigPlan = pInstruction `sepBy1'` endOfLine <* endOfLine <* endOfInput

type Trench = [(Ix2, RGB)]

excavate :: [Instruction] -> Trench
excavate = snd . foldl accF (0 :. 0, [])
  where
    accF (pos, trench) (Instruction dir len col) =
      let poss = getPositions pos dir len
          trench' = trench ++ [(p, col) | p <- getPositions pos dir len]
       in (last poss, trench')
    getPositions pos dir len =
      [ moveNStepsInDirection (fromIntegral n) pos dir
        | n <- [1 .. len]
      ]

getDimensions :: Trench -> (Ix2, Ix2)
getDimensions xs = (topLeft, bottomRight)
  where
    ks = map fst xs
    ms = map (fst . A.fromIx2) ks
    ns = map (snd . A.fromIx2) ks
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

ixsRightOf :: Sz Ix2 -> Set Ix2 -> Ix2 -> Direction -> [Ix2]
ixsRightOf sz lp ix dir = raytraceUntil sz lp ix (succWrap dir)

ixsLeftOf :: Sz Ix2 -> Set Ix2 -> Ix2 -> Direction -> [Ix2]
ixsLeftOf sz lp ix dir = raytraceUntil sz lp ix (predWrap dir)

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

main :: IO ()
main = do
  is <- parseChallengeT (Full 18) pDigPlan
  let trenchShifted = excavate is
      (topLeftShifted, _) = getDimensions trenchShifted
      trench = map (\(k, c) -> (k - topLeftShifted, c)) trenchShifted
      (_, bottomRight) = getDimensions trench
      trenchSize = Sz $ bottomRight + (1 :. 1)
      trenchPath = map fst trench
      trenchPathLoop = last trenchPath : trenchPath
      trenchPathLoopWithDirection = getLoopWithDirections trenchPathLoop
      (leftTiles, rightTiles) =
        bimap nub nub $
          getRightAndLeftTiles trenchSize (S.fromList trenchPathLoop) trenchPathLoopWithDirection
  print $ (+ length trench) $ length $ case (isOutside trenchSize leftTiles, isOutside trenchSize rightTiles) of
    (True, False) -> rightTiles
    (False, True) -> leftTiles
    _ -> error "could not determine inside"
