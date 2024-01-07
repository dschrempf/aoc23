{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Main
-- Description :  Day 19
-- Copyright   :  2023 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Mon Nov  6 05:48:34 2023.
--
-- See https://adventofcode.com/2023/day/19.
module Main
  ( main,
  )
where

import Aoc
import Control.Applicative (Alternative (..))
import Data.Attoparsec.Text
  ( Parser,
    char,
    choice,
    count,
    decimal,
    endOfInput,
    endOfLine,
    sepBy1',
    takeWhile1,
  )
import Data.Char (isAlpha)
import Data.Foldable (asum)
import Data.List (nub, sort)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe)
import Data.Set (Set)
import qualified Data.Set as S

data Category = X | M | A | S
  deriving (Show, Eq, Ord, Enum, Bounded)

pCategory :: Parser Category
pCategory =
  choice
    [ X <$ char 'x',
      M <$ char 'm',
      A <$ char 'a',
      S <$ char 's'
    ]

type Part = Category -> Int

pProperty :: Parser (Category, Int)
pProperty = do
  cat <- pCategory
  _ <- char '='
  val <- decimal
  pure (cat, val)

toPart :: [(Category, Int)] -> Part
toPart xs cat
  | allCategories cats = m M.! cat
  | otherwise = error "toPart: not all categories present for part"
  where
    allCategories ys = nub (sort ys) == [minBound .. maxBound]
    cats = map fst xs
    m = M.fromList xs

pPart :: Parser Part
pPart = do
  _ <- char '{'
  ps <- pProperty `sepBy1'` char ','
  _ <- char '}'
  pure $ toPart ps

pParts :: Parser [Part]
pParts = pPart `sepBy1'` endOfLine

type WorkflowName = Text

-- type Rule = Part -> Maybe WorkflowName

data Rule = Rule
  { category :: Category,
    comparison :: Comparison,
    thresholdValue :: Int,
    targetWorkflow :: WorkflowName
  }

data Workflow = Workflow
  { name :: WorkflowName,
    rules :: [Rule],
    catchall :: WorkflowName
  }

data Comparison = Less | Greater

pComparison :: Parser Comparison
pComparison = Less <$ char '<' <|> Greater <$ char '>'

pWorkflowName :: Parser WorkflowName
pWorkflowName = takeWhile1 isAlpha

pRule :: Parser Rule
pRule = Rule <$> pCategory <*> pComparison <*> decimal <*> (char ':' *> pWorkflowName)

pCatchAll :: Parser WorkflowName
pCatchAll = pWorkflowName

pWorkflow :: Parser Workflow
pWorkflow = do
  n <- pWorkflowName
  _ <- char '{'
  rs <- pRule `sepBy1'` char ','
  _ <- char ','
  ca <- pCatchAll
  _ <- char '}'
  pure $ Workflow n rs ca

type Workflows = Map WorkflowName Workflow

pWorkflows :: Parser Workflows
pWorkflows = M.fromList . map extractWorkflowName <$> pWorkflow `sepBy1'` endOfLine
  where
    extractWorkflowName w = (name w, w)

pInput :: Parser (Workflows, [Part])
pInput = do
  wfs <- pWorkflows
  _ <- count 2 endOfLine
  ps <- pParts
  _ <- endOfLine
  _ <- endOfInput
  pure (wfs, ps)

applyRule :: Rule -> Part -> Maybe WorkflowName
applyRule rule part
  | part rule.category `cmpOp` rule.thresholdValue = Just rule.targetWorkflow
  | otherwise = Nothing
  where
    cmpOp = case rule.comparison of
      Less -> (<)
      Greater -> (>)

rate1 :: Workflow -> Part -> WorkflowName
rate1 (Workflow _ rs ca) p = fromMaybe ca $ asum [applyRule r p | r <- rs]

rate :: Workflows -> Part -> Int
rate = go "in"
  where
    go source ws p
      | target == "R" = 0
      | target == "A" = sum [p cat | cat <- [minBound .. maxBound]]
      | otherwise = go target ws p
      where
        target = rate1 (ws M.! source) p

type Interval = (Int, Int)

data Tesseract = Tesseract
  { xdim :: Interval,
    mdim :: Interval,
    adim :: Interval,
    sdim :: Interval
  }
  deriving (Show, Eq, Ord)

computeAcceptedTesseracts :: Workflow -> Tesseract -> Set Tesseract
computeAcceptedTesseracts (Workflow _ rs ca) (Tesseract x m a s) = undefined

main :: IO ()
main = do
  (workflows, parts) <- parseChallengeT (Full 19) pInput
  print $ sum $ map (rate workflows) parts
