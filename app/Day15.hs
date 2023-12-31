{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Main
-- Description :  Day 15
-- Copyright   :  2023 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Mon Nov  6 05:48:34 2023.
--
-- See https://adventofcode.com/2023/day/15.
module Main
  ( main,
  )
where

import Aoc
import Control.Applicative (Alternative (..))
import Data.Attoparsec.Text (Parser, char, decimal, sepBy1', takeWhile1)
import Data.Char (ord)
import Data.Foldable (Foldable (..))
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import qualified Data.Text as T

pInput1 :: Parser [Text]
pInput1 = takeWhile1 (`notElem` [',', '\n']) `sepBy1'` char ','

hashChar :: Int -> Char -> Int
hashChar n c = (n + ord c) * 17 `mod` 256

hash :: Text -> Int
hash = T.foldl hashChar 0

data Lens = Lens {label :: Text, focalLength :: Int}
  deriving (Show)

type Box = Seq Lens

type Boxes = Seq Box

data Instruction
  = Remove
      { box :: Int,
        lens :: Text
      }
  | AddReplace
      { box :: Int,
        lens :: Text,
        focalLength :: Int
      }
  deriving (Show)

pInstruction :: Parser Instruction
pInstruction = do
  lensLabel <- takeWhile1 (`notElem` [',', '\n', '-', '='])
  let boxNumber = hash lensLabel
      pRemove = char '-' >> pure (Remove boxNumber lensLabel)
      pAddReplace = char '=' *> (AddReplace boxNumber lensLabel <$> decimal)
  pRemove <|> pAddReplace

pInput2 :: Parser [Instruction]
pInput2 = pInstruction `sepBy1'` char ','

runInstruction :: Boxes -> Instruction -> Boxes
runInstruction bs (Remove b l) = S.adjust removeLens b bs
  where
    removeLens ls =
      case S.findIndexL ((== l) . label) ls of
        Nothing -> ls
        (Just i) -> S.deleteAt i ls
runInstruction bs (AddReplace b l f) = S.adjust addReplaceLens b bs
  where
    addReplaceLens ls =
      case S.findIndexL ((== l) . label) ls of
        Nothing -> ls S.|> Lens l f
        (Just i) -> S.update i (Lens l f) ls

runInstructions :: [Instruction] -> Boxes
runInstructions = foldl runInstruction emptyBoxes
  where
    emptyBoxes = S.replicate 256 S.empty

gradeLens :: Int -> Int -> Lens -> Int
gradeLens boxN lensN (Lens _ f) = boxN * lensN * f

gradeBox :: Int -> Box -> Int
gradeBox boxN ls = sum $ zipWith (gradeLens boxN) [1 ..] $ toList ls

grade :: Boxes -> Int
grade bs = sum $ zipWith gradeBox [1 ..] $ toList bs

main :: IO ()
main = do
  d1 <- parseChallengeT (Full 15) pInput1
  print $ sum $ map hash d1
  d2 <- parseChallengeT (Full 15) pInput2
  print $ grade $ runInstructions d2
