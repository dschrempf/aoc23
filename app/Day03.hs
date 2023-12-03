-- |
-- Module      :  Main
-- Description :  Day 3
-- Copyright   :  2023 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Mon Nov  6 05:48:34 2023.
--
-- See https://adventofcode.com/2023/day/3.
module Main
  ( main,
  )
where

import Aoc (parseChallengeT)
import Aoc.Array (filterA, neighbors)
import Aoc.Char (readDigit)
import Aoc.Def (Challenge (..))
import Control.Applicative (Alternative (..), optional, (<|>))
import Data.Attoparsec.Text
  ( Parser,
    char,
    digit,
    endOfInput,
    endOfLine,
    satisfy,
    sepBy1',
  )
import Data.Char (isPrint)
import Data.Ix (Ix (..))
import Data.List (find, nub)
import Data.Massiv.Array (Ix2 (..))
import qualified Data.Massiv.Array as A
import Data.Maybe (mapMaybe)

data Elem = Dot | Digit Int | Asterisk | OtherSymbol
  deriving (Show, Eq)

pEl :: Parser Elem
pEl = dot <|> dig <|> ast <|> sym
  where
    dot = Dot <$ char '.'
    dig = Digit . readDigit <$> digit
    ast = Asterisk <$ char '*'
    sym = OtherSymbol <$ satisfy isPrint

type Schematic = A.Array A.B A.Ix2 Elem

pSchematic :: Parser Schematic
pSchematic =
  A.fromLists' A.Seq
    <$> some pEl `sepBy1'` endOfLine
    <* optional endOfLine
    <* endOfInput

data Number = Number {index :: A.Ix2, digits :: Int, number :: Int}
  deriving (Show, Eq)

data FoldState = FNoNumber | FNumber Number

getNumbers :: Schematic -> [Number]
getNumbers = concat . A.toLists . A.map fromAcc . A.ifoldlInner accF ([], FNoNumber)
  where
    accF ix (xs, FNoNumber) (Digit n) = (xs, FNumber $ Number ix 1 n)
    accF _ (xs, FNoNumber) _ = (xs, FNoNumber)
    accF _ (xs, FNumber (Number ix ds n)) (Digit m) =
      (xs, FNumber $ Number ix (succ ds) (n * 10 + m))
    accF _ (xs, FNumber x) _ = (x : xs, FNoNumber)
    fromAcc (xs, FNoNumber) = xs
    fromAcc (xs, FNumber x) = x : xs

hasSymbolNeighbor :: Schematic -> A.Ix2 -> Bool
hasSymbolNeighbor xs i =
  or
    [ x == OtherSymbol || x == Asterisk
      | n <- neighbors s i,
        let x = xs A.! n
    ]
  where
    s = A.size xs

getIxRange :: Number -> [A.Ix2]
getIxRange (Number ix d _) = range (ix, ix')
  where
    (i :. j) = ix
    ix' = i :. j + d - 1

isPartNumberWith :: Schematic -> Number -> Bool
isPartNumberWith xs x = any (hasSymbolNeighbor xs) (getIxRange x)

getNumberAtIx :: [Number] -> A.Ix2 -> Maybe Number
getNumberAtIx xs ix = find numberOverlapsWithIx xs
  where
    numberOverlapsWithIx x = ix `elem` getIxRange x

getNumberNeighborsWith :: Schematic -> [Number] -> A.Ix2 -> [Number]
getNumberNeighborsWith schematic xs ix =
  nub $
    mapMaybe (getNumberAtIx xs) $
      neighbors (A.size schematic) ix

getAsterisks :: Schematic -> [A.Ix2]
getAsterisks = filterA (== Asterisk)

gearRatio :: [Number] -> Integer
gearRatio [Number _ _ n, Number _ _ m] = fromIntegral n * fromIntegral m
gearRatio _ = 0

main :: IO ()
main = do
  d <- parseChallengeT (Full 3) pSchematic
  let ns = getNumbers d
  print $ sum $ map number $ filter (isPartNumberWith d) ns
  print $ sum $ map (gearRatio . getNumberNeighborsWith d ns) $ getAsterisks d
