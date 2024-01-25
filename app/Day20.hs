{-# LANGUAGE DeriveGeneric #-}
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
import Aoc.Bounded (succWrap)
import Aoc.Function (nTimesStrict)
import Control.Applicative (Alternative (..))
import Control.DeepSeq
import Data.Attoparsec.Text
  ( Parser,
    char,
    endOfLine,
    sepBy1',
    string,
    takeWhile1,
  )
import Data.Char (isAlpha)
import Data.Foldable (Foldable (..))
import Data.List (nub, partition)
import Data.Map (Map)
import qualified Data.Map as M
import GHC.Generics (Generic)

data PulseType = Low | High
  deriving (Show, Eq, Ord, Generic)

instance NFData PulseType

data FlipFlopState = On | Off
  deriving (Show, Eq, Enum, Bounded, Generic)

instance NFData FlipFlopState

type ModuleName = Text

data ModuleType
  = Broadcaster
  | FlipFlop FlipFlopState
  | Conjunction (Map ModuleName PulseType)
  deriving (Show, Eq, Generic)

instance NFData ModuleType

data Module = Module
  { moduleType :: ModuleType,
    moduleName :: ModuleName,
    destinations :: [ModuleName]
  }
  deriving (Show, Eq, Generic)

instance NFData Module

pModuleName :: Parser Text
pModuleName = takeWhile1 isAlpha

pDestinations :: Parser [ModuleName]
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

type Modules = Map ModuleName Module

isConjunction :: ModuleType -> Bool
isConjunction (Conjunction _) = True
isConjunction _ = False

connectConjunction :: Module -> [Module] -> Module
connectConjunction (Module (Conjunction _) n ds) is = Module (Conjunction ss') n ds
  where
    ss' = M.fromList $ [(moduleName i, Low) | i <- is]
connectConjunction _ _ = error "bug"

connectConjunctions :: Modules -> Modules
connectConjunctions ms = foldl' connectAndInsertConjunction ms cs
  where
    cs = M.filter (isConjunction . moduleType) ms
    msWithCAsDestination c = filter (elem (moduleName c) . destinations) $ M.elems ms
    connectAndInsertConjunction ys c =
      let c' = connectConjunction c (msWithCAsDestination c)
       in M.insert (moduleName c) c' ys

pInput :: Parser Modules
pInput =
  connectConjunctions
    . M.fromList
    . map extractModuleName
    <$> pModule `sepBy1'` endOfLine
  where
    extractModuleName m = (moduleName m, m)

data Pulse = Pulse
  { source :: ModuleName,
    destination :: ModuleName,
    pulseType :: PulseType
  }
  deriving (Show, Generic)

instance NFData Pulse

data State = State
  { modules :: Modules,
    _pulsesInLine :: [Pulse],
    pulsesSent :: [Pulse]
  }

press :: (Modules, [Pulse]) -> (Modules, [Pulse])
press (ms, ps) =
  let p = Pulse "button" "broadcaster" Low
      s' = pulses $ State ms [p] [p]
   in (modules s', ps <> pulsesSent s')

pulses :: State -> State
pulses s@(State _ [] _) = s
pulses (State ms (x : xs) ps) =
  let (ms', ys) = pulse x ms
   in pulses $
        State ms' (xs <> ys) $
          ps <> ys

pulse :: Pulse -> Modules -> (Modules, [Pulse])
pulse p ms = (M.insert d m' ms, ps)
  where
    d = destination p
    m = case ms M.!? d of
      Nothing -> Module Broadcaster d []
      Just x -> x
    (m', ps) = pulse1 p m

pulse1 :: Pulse -> Module -> (Module, [Pulse])
pulse1 (Pulse _ _ t) m@(Module Broadcaster n ds) = (m, [Pulse n d t | d <- ds])
pulse1 p m@(Module (FlipFlop s) n ds) = case pulseType p of
  High -> (m, [])
  Low ->
    let s' = succWrap s
        t = case s' of
          On -> High
          Off -> Low
     in (Module (FlipFlop s') n ds, [Pulse n d t | d <- ds])
pulse1 p (Module (Conjunction ss) n ds) =
  let ss' = M.insert (source p) (pulseType p) ss
      t = if nub (M.elems ss') == [High] then Low else High
   in (Module (Conjunction ss') n ds, [Pulse n d t | d <- ds])

main :: IO ()
main = do
  ms <- parseChallengeT (Full 20) pInput
  print ms
  let (_, ps) = nTimesStrict 1000 press (ms, [])
  let (ls, hs) = partition (\(Pulse _ _ x) -> x == Low) ps
  print $ length ls
  print $ length hs
  print $ length ls * length hs
