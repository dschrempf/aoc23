-- |
-- Module      :  Main
-- Description :  Day 12
-- Copyright   :  2023 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Mon Nov  6 05:48:34 2023.
--
-- See https://adventofcode.com/2023/day/12.
module Main
  ( main,
  )
where

import Aoc
import Aoc.Parse (skipHorizontalSpace)
import Control.Applicative (Alternative (..))
import Data.Attoparsec.Text
  ( Parser,
    char,
    choice,
    decimal,
    endOfInput,
    endOfLine,
    sepBy1',
  )
import Data.Function.Memoize (Memoizable (..), memoize2, memoizeFinite)
import Data.List (intercalate)
import Data.Maybe (listToMaybe)

data Spring = Operational | Damaged | Unknown
  deriving (Eq, Bounded, Enum)

instance Memoizable Spring where memoize = memoizeFinite

instance Show Spring where
  show Operational = "."
  show Damaged = "#"
  show Unknown = "?"

pSpring :: Parser Spring
pSpring = choice [o, d, u]
  where
    o = Operational <$ char '.'
    d = Damaged <$ char '#'
    u = Unknown <$ char '?'

type Group = Int

pGroups :: Parser [Group]
pGroups = decimal `sepBy1'` char ','

type Line = ([Spring], [Group])

pLine :: Parser Line
pLine = (,) <$> some pSpring <* skipHorizontalSpace <*> pGroups

pInput :: Parser [Line]
pInput = pLine `sepBy1'` endOfLine <* endOfLine <* endOfInput

expand :: Line -> Line
expand (ss, gs) = (intercalate [Unknown] sss, concat $ replicate 5 gs)
  where
    sss = replicate 5 ss

compatible :: Group -> [Spring] -> Bool
compatible n xs = length ys == n && Operational `notElem` ys && isOperationalLike h
  where
    ys = take n xs
    h = listToMaybe $ take 1 $ drop n xs

isOperationalLike :: Maybe Spring -> Bool
isOperationalLike Nothing = True
isOperationalLike (Just Operational) = True
isOperationalLike (Just Unknown) = True
isOperationalLike (Just Damaged) = False

use :: Group -> [Spring] -> [Spring]
use n = drop (succ n)

solve :: [Spring] -> [Group] -> Int
solve [] (_ : _) = 0
solve xs []
  | Damaged `elem` xs = 0
  | otherwise = 1
solve (Unknown : Unknown : Unknown : rs) gs =
  memoize2 solve (Unknown : Unknown : Operational : rs) gs
    + memoize2 solve (Unknown : Unknown : Damaged : rs) gs
solve xs@(Unknown : Unknown : Operational : _) (1 : gs) =
  2 * memoize2 solve (use 2 xs) gs + memoize2 solve (use 2 xs) (1 : gs)
solve xs gs | length xs < sum gs + length gs - 1 = 0
solve xs@(Unknown : rs) (g : gs)
  | compatible g xs = memoize solve (use g xs) gs + solve rs (g : gs)
  | otherwise = solve rs (g : gs)
solve (Operational : rs) (g : gs) = solve rs (g : gs)
solve xs@(Damaged : _) (g : gs)
  | compatible g xs = memoize solve (use g xs) gs
  | otherwise = 0

main :: IO ()
main = do
  ls <- parseChallengeT (Full 12) pInput
  print $ sum $ map (uncurry solve) ls
  let ls2 = map expand ls
  print $ sum $ map (uncurry (memoize2 solve)) ls2
