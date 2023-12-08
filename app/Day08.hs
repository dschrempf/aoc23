{-# LANGUAGE BangPatterns #-}

-- |
-- Module      :  Main
-- Description :  Day 8
-- Copyright   :  2023 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Mon Nov  6 05:48:34 2023.
--
-- See https://adventofcode.com/2023/day/8.
module Main
  ( main,
  )
where

import Aoc (parseChallengeT)
import Aoc.Def (Challenge (..))
import Control.Applicative (Alternative (..))
import Data.Attoparsec.Text
  ( Parser,
    char,
    count,
    digit,
    endOfInput,
    endOfLine,
    letter,
    sepBy1',
    skipSpace,
  )
import qualified Data.Map.Strict as M

data Direction = L | R
  deriving (Show, Eq)

pDirs :: Parser [Direction]
pDirs = some dir
  where
    r = R <$ char 'R'
    l = L <$ char 'L'
    dir = r <|> l

type Node = String

pNode :: Parser Node
pNode = count 3 (letter <|> digit)

data Destination = Destination {left :: Node, right :: Node}
  deriving (Show)

pDestination :: Parser Destination
pDestination =
  Destination
    <$> (char '(' *> pNode <* char ',' <* skipSpace)
    <*> (pNode <* char ')')

data Edge = Edge {start :: Node, destination :: Destination}
  deriving (Show)

pEdge :: Parser Edge
pEdge =
  Edge
    <$> (pNode <* skipSpace <* char '=' <* skipSpace)
    <*> pDestination

pInput :: Parser ([Direction], [Edge])
pInput = do
  dirs <- pDirs
  endOfLine
  endOfLine
  edges <- pEdge `sepBy1'` endOfLine
  endOfLine
  endOfInput
  pure (dirs, edges)

type Network = M.Map Node Destination

toNetwork :: [Edge] -> Network
toNetwork = foldl insertEdge M.empty
  where
    insertEdge m (Edge s d) = M.insert s d m

getDestination :: Direction -> Destination -> Node
getDestination L = left
getDestination R = right

jump :: Direction -> Network -> Node -> Node
jump dir net node = getDestination dir $ net M.! node

walk :: Network -> [Direction] -> Node -> (Node, Int)
walk net dirs s = go s 1 dirs
  where
    go n l (d : ds) =
      let !n' = jump d net n
       in if last n' == 'Z'
            then (n', l)
            else let !l' = succ l in go n' l' ds
    go n l [] = go n l dirs

main :: IO ()
main = do
  (dirs, edges) <- parseChallengeT (Full 8) pInput
  let net = toNetwork edges
  print $ snd $ walk net dirs "AAA"
  let starts = filter ((== 'A') . last) $ map start edges
      firstEnd = map (walk net dirs) starts
  print $ foldl lcm 1 $ map snd firstEnd
