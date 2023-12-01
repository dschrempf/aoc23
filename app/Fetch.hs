{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Aoc.Def (Challenge (..), ChallengeType (..), getInputFile, year)
import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Monad.Catch (MonadThrow)
import qualified Data.ByteString as B
import Data.ByteString.Char8 (pack)
import Network.HTTP.Conduit (Request)
import Network.HTTP.Simple (addRequestHeader, getResponseBody, httpBS, parseRequest)
import System.Directory (doesFileExist, getFileSize)
import System.Environment (getArgs, lookupEnv)

baseUrl :: String
baseUrl = "https://adventofcode.com/" <> show year <> "/day/"

getRoute :: Challenge -> String
getRoute (Challenge d Full) = baseUrl <> show d <> "/input"
getRoute challenge = baseUrl <> show (challengeDay challenge)

getRequest :: (MonadThrow m) => String -> Challenge -> m Request
getRequest token challenge = do
  req <- parseRequest (getRoute challenge)
  return $ addRequestHeader "cookie" (pack token) req

fetchInput :: Challenge -> IO ()
fetchInput challenge = do
  isCached <- checkFileExistsWithData file
  token' <- lookupEnv "AOC_TOKEN"
  case (isCached, token') of
    (True, _) -> putStrLn "Input has been downloaded already."
    (False, Nothing) -> putStrLn "No session token."
    (False, Just token) -> do
      req <- getRequest token challenge
      response <- getResponseBody <$> httpBS req
      B.writeFile file response
  where
    file = getInputFile challenge

checkFileExistsWithData :: FilePath -> IO Bool
checkFileExistsWithData file = do
  exists <- doesFileExist file
  if not exists
    then return False
    else do
      size <- getFileSize file
      return $ size > 0

main :: IO ()
main = do
  loadFile defaultConfig
  [day] <- map read <$> getArgs
  fetchInput (Challenge day Full)
