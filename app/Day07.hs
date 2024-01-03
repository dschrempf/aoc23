-- |
-- Module      :  Main
-- Description :  Day 7
-- Copyright   :  2023 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Mon Nov  6 05:48:34 2023.
--
-- See https://adventofcode.com/2023/day/7.
module Main
  ( main,
  )
where

import Aoc
import Aoc.Occurrence (countOccurrences)
import Control.Applicative (Alternative (..), (<|>))
import Data.Attoparsec.Text (Parser, char, decimal, endOfInput, endOfLine, sepBy1', skipSpace)
import Data.List (sort, sortBy, sortOn)
import qualified Data.Map.Strict as M
import Data.Ord (Down (..), comparing)

data Card = J | C2 | C3 | C4 | C5 | C6 | C7 | C8 | C9 | T | Q | K | A
  deriving (Show, Enum, Eq, Ord)

pCard :: Parser Card
pCard = a <|> k <|> q <|> j <|> t <|> c9 <|> c8 <|> c7 <|> c6 <|> c5 <|> c4 <|> c3 <|> c2
  where
    a = A <$ char 'A'
    k = K <$ char 'K'
    q = Q <$ char 'Q'
    j = J <$ char 'J'
    t = T <$ char 'T'
    c9 = C9 <$ char '9'
    c8 = C8 <$ char '8'
    c7 = C7 <$ char '7'
    c6 = C6 <$ char '6'
    c5 = C5 <$ char '5'
    c4 = C4 <$ char '4'
    c3 = C3 <$ char '3'
    c2 = C2 <$ char '2'

newtype Hand = Hand {cards :: [Card]}
  deriving (Show, Eq)

data Type
  = HighCard
  | OnePair
  | TwoPair
  | ThreeOfAKind
  | FullHouse
  | FourOfAKind
  | FiveOfAKind
  deriving (Show, Eq, Ord)

getType :: Hand -> Type
getType (Hand cs)
  | head counts == 5 = FiveOfAKind
  | counts == [4, 1] = FourOfAKind
  | counts == [3, 2] = FullHouse
  | head counts == 3 = ThreeOfAKind
  | head counts == 2 && counts !! 1 == 2 = TwoPair
  | head counts == 2 = OnePair
  | otherwise = HighCard
  where
    nJs = length $ filter (== J) cs
    csNoJ = filter (/= J) cs
    sorted = sort csNoJ
    occurences = M.elems $ countOccurrences sorted
    counts' = sortBy (comparing Down) occurences
    counts
      | null counts' = [5]
      | otherwise = (head counts' + nJs) : tail counts'

instance Ord Hand where
  l `compare` r = case lT `compare` rT of
    LT -> LT
    GT -> GT
    EQ -> cards l `compare` cards r
    where
      lT = getType l
      rT = getType r

pHand :: Parser Hand
pHand = Hand <$> some pCard

type Bid = Int

pBid :: Parser Bid
pBid = decimal

data Move = Move {hand :: Hand, bid :: Bid}
  deriving (Show)

pMove :: Parser Move
pMove = Move <$> (pHand <* skipSpace) <*> pBid

type Game = [Move]

pGame :: Parser Game
pGame = pMove `sepBy1'` endOfLine <* endOfLine <* endOfInput

main :: IO ()
main = do
  d <- parseChallengeT (Full 7) pGame
  let ss = sortOn hand d
  putStrLn "This only solves part 2, because the Ord types are different."
  print $ sum $ zipWith (\rank (Move _ b) -> rank * b) [1 ..] ss
