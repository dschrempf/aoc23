{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Main
-- Description :  Day 2
-- Copyright   :  2023 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Mon Nov  6 05:48:34 2023.
--
-- See https://adventofcode.com/2023/day/2.
module Main
  ( main,
  )
where

import Aoc (Challenge (..), parseChallengeT)
import Control.Applicative ((<|>))
import Data.Attoparsec.Text (Parser, char, decimal, endOfInput, endOfLine, sepBy1', skipSpace, string)
import Data.Foldable (Foldable (..))
import Numeric.Natural (Natural)

data Color = Red | Green | Blue
  deriving (Show)

pColor :: Parser Color
pColor = r <|> g <|> b
  where
    r = Red <$ string "red"
    g = Green <$ string "green"
    b = Blue <$ string "blue"

pColorN :: Parser (Color, Natural)
pColorN = do
  n <- decimal
  skipSpace
  c <- pColor
  return (c, n)

data Draw = Draw {nRed :: Natural, nGreen :: Natural, nBlue :: Natural}
  deriving (Show)

pDraw :: Parser Draw
pDraw = foldl' fillDraw (Draw 0 0 0) <$> pColorN `sepBy1'` string ", "
  where
    fillDraw (Draw r g b) (Red, n) = Draw (r + n) g b
    fillDraw (Draw r g b) (Green, n) = Draw r (g + n) b
    fillDraw (Draw r g b) (Blue, n) = Draw r g (b + n)

data Game = Game {gameId :: Natural, gameDraws :: [Draw]}
  deriving (Show)

pGame :: Parser Game
pGame = do
  _ <- string "Game"
  skipSpace
  n <- decimal
  _ <- char ':'
  skipSpace
  ds <- pDraw `sepBy1'` string "; "
  return $ Game n ds

pInput :: Parser [Game]
pInput = pGame `sepBy1'` endOfLine <* endOfLine <* endOfInput

-- cubesAvailable :: Draw
-- cubesAvailable = Draw 12 13 14

possibleDraw :: Draw -> Bool
possibleDraw (Draw r g b) = r <= 12 && g <= 13 && b <= 14

possibleGame :: Game -> Bool
possibleGame (Game _ xs) = all possibleDraw xs

possibleGames :: [Game] -> [Game]
possibleGames = filter possibleGame

minDraw :: [Draw] -> Draw
minDraw xs = Draw (getMaxCubes nRed) (getMaxCubes nGreen) (getMaxCubes nBlue)
  where
    getMaxCubes f = maximum $ map f xs

power :: Draw -> Natural
power (Draw r g b) = r * g * b

main :: IO ()
main = do
  d <- parseChallengeT (Full 2) pInput
  let xs = possibleGames d
  print $ sum $ map gameId xs
  print $ sum $ map (power . minDraw . gameDraws) d
