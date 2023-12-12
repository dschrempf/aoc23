{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

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
import Control.Concurrent.Async (mapConcurrently_)
import Control.DeepSeq (NFData, force)
import Data.Attoparsec.Text (Parser, char, choice, decimal, endOfInput, endOfLine, sepBy1')
import Data.List (intercalate)
import Data.Maybe (fromMaybe, listToMaybe)
import GHC.Generics (Generic)

data Spring = Operational | Damaged | Unknown
  deriving (Eq, Generic, NFData)

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

-- springsToGroups :: [Spring] -> [(Group, Spring)]
-- springsToGroups [] = []
-- springsToGroups (x : xs) = go (1, x) xs
--   where
--     go (n, y) [] = [(n, y)]
--     go (n, y) (z : zs)
--       | z == y = go (succ n, y) zs
--       | otherwise = (n, y) : go (1, z) zs

-- damagedGroups :: [(Group, Spring)] -> [Group]
-- damagedGroups = map fst . filter ((== Damaged) . snd)

-- configurations :: [Spring] -> [[Spring]]
-- configurations [] = [[]]
-- configurations (Unknown : xs) = let cs = force $ configurations xs in map (Operational :) cs ++ map (Damaged :) cs
-- configurations (x : xs) = map (x :) $ configurations xs

-- solve :: Line -> Int
-- solve (ss, gs) = length $ filter (== gs) gss'
--   where
--     cs = configurations ss
--     gss' = map (damagedGroups . springsToGroups) cs

expand :: Line -> Line
expand (ss, gs) = (intercalate [Unknown] sss, concat $ replicate 5 gs)
  where
    sss = replicate 5 ss

-- solve2 :: [Spring] -> [Group] -> Int
-- solve2 = go Nothing
--   where
--     -- Run out of springs.
--     go Nothing [] [] = 1
--     go (Just n) [] [g] | g == n = 1
--     go _ [] _ = 0
--     -- Iterate springs.
--     go Nothing (Operational : xs) gs = go Nothing xs gs
--     go Nothing (Damaged : xs) gs = go (Just 1) xs gs
--     go mn (Unknown : xs) gs = force (go mn (Operational : xs) gs) + force (go mn (Damaged : xs) gs)
--     go (Just n) (Operational : xs) gs
--       | listToMaybe gs == Just n = go Nothing xs $ tail gs
--       | otherwise = 0
--     go (Just _) (Damaged : _) [] = 0
--     go (Just n) (Damaged : xs) (g : gs)
--       | n > g = 0
--       | otherwise = go (Just $ succ n) xs (g : gs)

-- solve2 :: [Spring] -> [Group] -> Int
-- solve2 ss groups = go (sum groups) (length $ filter (/= Operational) ss) Nothing ss groups
--   where
--     go :: Int -> Int -> Maybe Int -> [Spring] -> [Group] -> Int
--     -- Run out of springs.
--     go _ _ Nothing [] [] = 1
--     go _ _ (Just n) [] [g] | g == n = 1
--     go _ _ _ [] _ = 0
--     -- Iterate springs.
--     go gsum l Nothing (Operational : xs) gs = go gsum l Nothing xs gs
--     go gsum l Nothing (Damaged : xs) gs = go gsum (pred l) (Just 1) xs gs
--     go gsum l mn (Unknown : xs) gs =
--       force
--         ( if pred l + fromMaybe 0 mn < gsum
--             then 0
--             else go gsum (pred l) mn (Operational : xs) gs
--         )
--         + force (go gsum l mn (Damaged : xs) gs)
--     go _ l (Just n) (Operational : xs) gs
--       | listToMaybe gs == Just n = go (sum $ tail gs) l Nothing xs $ tail gs
--       | otherwise = 0
--     go _ _ (Just _) (Damaged : _) [] = 0
--     go gsum l (Just n) (Damaged : xs) (g : gs)
--       | n > g = 0
--       | otherwise = go gsum (pred l) (Just $ succ n) xs (g : gs)

solve2 :: [Spring] -> [Group] -> Int
solve2 ss groups = go (length ss) (sum groups) (length $ filter (/= Operational) ss) Nothing ss groups
  where
    go :: Int -> Int -> Int -> Maybe Int -> [Spring] -> [Group] -> Int
    -- Run out of springs.
    go _ _ _ Nothing [] [] = 1
    go _ _ _ (Just n) [] [g] | g == n = 1
    go _ _ _ _ [] _ = 0
    -- Iterate springs.
    go len gsum l Nothing (Operational : xs) gs = go (pred len) gsum l Nothing xs gs
    go len gsum l Nothing (Damaged : xs) gs = go (pred len) gsum (pred l) (Just 1) xs gs
    go len gsum l Nothing (Unknown : Unknown : Operational : xs) (1 : gs) =
      2 * go (len - 2) (pred gsum) (l - 2) Nothing (Operational : xs) gs
        + go (len - 2) gsum (l - 2) Nothing (Operational : xs) (1 : gs)
    go len gsum l mn (Unknown : xs) gs =
      force
        ( if pred l + fromMaybe 0 mn < gsum
            then 0
            else go len gsum (pred l) mn (Operational : xs) gs
        )
        + force (go len gsum l mn (Damaged : xs) gs)
    go len _ l (Just n) (Operational : xs) gs
      | listToMaybe gs == Just n = go (pred len) (sum $ tail gs) l Nothing xs $ tail gs
      | otherwise = 0
    go _ _ _ (Just _) (Damaged : _) [] = 0
    go len gsum l (Just n) (Damaged : xs) (g : gs)
      | n > g = 0
      | otherwise = go (pred len) gsum (pred l) (Just $ succ n) xs (g : gs)

main :: IO ()
main = do
  ls <- parseChallengeT (Full 12) pInput
  print $ sum $ map (uncurry solve2) ls
  let ls2 = map expand ls
  -- print $ sum $ map (uncurry solve2) ls2
  -- mapConcurrently_ (print . uncurry solve2) ls2
  mapM_ (print . uncurry solve2) ls2
