-- |
-- Module      :  Main
-- Description :  Day 21
-- Copyright   :  2023 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Mon Nov  6 05:48:34 2023.
--
-- See https://adventofcode.com/2023/day/21.
module Main
  ( main,
  )
where

import Aoc
import Aoc.Array (neighborsNoDiagonal, neighborsNoDiagonalInf, parseMatrix)
import Aoc.Function (nTimesStrict)
import Aoc.Set (flatten)
import Data.Attoparsec.Text (Parser)
import Data.List (zip4)
import Data.Massiv.Array (Array, B, Ix2 (..), Sz (..))
import qualified Data.Massiv.Array as A
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as S
import Text.Printf (printf)

type Rocks = Set Ix2

type Positions = Set Ix2

data Tile = Start | Plot | Rock
  deriving (Show, Eq)

pInput :: Parser (Array B Ix2 Tile)
pInput = parseMatrix [('S', Start), ('.', Plot), ('#', Rock)]

getRocks :: Array B Ix2 Tile -> Rocks
getRocks = A.ifoldlS addRock S.empty
  where
    addRock rocks ix Rock = S.insert ix rocks
    addRock rocks _ _ = rocks

getStart :: Array B Ix2 Tile -> Ix2
getStart = fromJust . A.findIndex (== Start)

moveAll :: Sz Ix2 -> Rocks -> Positions -> Positions
moveAll sz rs ps = ps' S.\\ rs
  where
    ps' = flatten $ S.map (S.fromList . neighborsNoDiagonal sz) ps

moveAllInf :: Sz Ix2 -> Rocks -> Positions -> Positions
moveAllInf (Sz (ht :. wd)) rs ps = S.filter isNoRock ps'
  where
    isNoRock (m :. n) = let ix' = (m `mod` ht) :. (n `mod` wd) in S.notMember ix' rs
    ps' = flatten $ S.map (S.fromList . neighborsNoDiagonalInf) ps

printSequence :: [Int] -> IO ()
printSequence xs =
  sequence_
    [ printf "%6d %6d %6d %6d %6d\n" i s s' (s' - s) (s'' - s)
      | (i, s, s', s'') <- zip4 [0 :: Int ..] xs (tail xs) (drop 2 xs)
    ]

everyNth :: Int -> [a] -> [a]
everyNth n xs
  | n < 1 = error "n zero or negative"
  | otherwise = case drop (pred n) xs of
      [] -> []
      (x : ys) -> x : everyNth n ys

everyNth' :: Int -> [a] -> [a]
everyNth' n xs
  | n < 1 = error "n zero or negative"
  | otherwise = case drop (pred n) xs of
      [] -> []
      (x : ys) -> x : everyNth n ys

tiles :: [Int]
tiles = 3941 : 35259 : go tiles
  where
    go (xI2 : xI1 : xs) = (31230 + 2 * xI1 - xI2) : go (xI1 : xs)
    go _ = error "not enough numbers"

main :: IO ()
main = do
  d <- parseChallengeT (Full 21) pInput
  -- Part 1.
  let size = A.size d
  print size
  let rocks = getRocks d
      start = getStart d
  let mv1 = moveAll size rocks
  print $ S.size $ nTimesStrict 64 mv1 (S.singleton start)
  -- Part 2.
  -- -- We need to get the first and second "derivative". I did this in
  -- -- Libreoffice using the following code:
  -- let mv2 = moveAllInf size rocks
  --     (Sz (ht :. _)) = size
  --     ss2 = map S.size $ take (succ $ 65 + 4 * ht) $ iterate mv2 (S.singleton start)
  -- mapM_ print ss2
  -- putStr "\n"
  -- let ss2' = drop 65 ss2
  -- print $ head ss2'
  -- mapM_ print $ everyNth ht $ tail ss2'
  print $ tiles !! 202300
