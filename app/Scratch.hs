-- |
-- Module      :  Main
-- Description :  Scratch
-- Copyright   :  2023 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Wed Nov  8 16:55:36 2023.
module Main
  ( main,
  )
where

import Aoc
import Aoc.Def (ChallengeType (..))
import Data.Attoparsec.Text
import Prelude hiding (takeWhile)

p :: Parser Text
p = takeWhile (const True)

main :: IO ()
main = do
  d <- parseChallengeT (Challenge 1 Full) p
  print d
