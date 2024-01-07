{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Main
-- Description :  Day 20
-- Copyright   :  2023 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Mon Nov  6 05:48:34 2023.
--
-- See https://adventofcode.com/2023/day/20.
module Main
  ( main,
  )
where

import Aoc
import Control.Applicative (Alternative (..))
import Data.Attoparsec.Text
  ( Parser,
    char,
    endOfLine,
    sepBy1',
    string,
    takeWhile1,
  )
import Data.Char (isAlpha)
import Data.Map (Map)
import qualified Data.Map as M

data Pulse = Low | High
  deriving (Show, Eq)

data FlipFlopState = On | Off
  deriving (Show, Eq)

type ModuleName = Text

type Input = Text

type Destination = Text

data ModuleType
  = Broadcaster
  | FlipFlop FlipFlopState
  | Conjunction (Map Input Pulse)
  deriving (Show, Eq)

data Module = Module
  { moduleType :: ModuleType,
    moduleName :: ModuleName,
    destinations :: [Destination]
  }
  deriving (Show, Eq)

pModuleName :: Parser Text
pModuleName = takeWhile1 isAlpha

pDestinations :: Parser [Destination]
pDestinations = pModuleName `sepBy1'` string ", "

pStandardModuleWith :: ModuleType -> Parser Module
pStandardModuleWith mt =
  Module mt
    <$> pModuleName
    <*> (string " -> " *> pDestinations)

pBroadcaster :: Parser Module
pBroadcaster = pStandardModuleWith Broadcaster

pFlipFlop :: Parser Module
pFlipFlop = char '%' *> pStandardModuleWith (FlipFlop Off)

pConjunction :: Parser Module
pConjunction = char '&' *> pStandardModuleWith (Conjunction M.empty)

pModule :: Parser Module
pModule = pBroadcaster <|> pFlipFlop <|> pConjunction

pInput :: Parser (Map ModuleName Module)
pInput = M.fromList . map extractModuleName <$> pModule `sepBy1'` endOfLine
  where
    extractModuleName m = (moduleName m, m)

main :: IO ()
main = do
  ms <- parseChallengeT (Sample 20 1) pInput
  print ms
