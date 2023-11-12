{-# LANGUAGE OverloadedStrings #-}

module Main where

import Aoc.Def (Challenge (F), Day, getInputFile, year)
import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Monad.Catch (MonadThrow)
import qualified Data.ByteString as B
import Data.ByteString.Char8 (pack)
import Network.HTTP.Conduit (Request)
import Network.HTTP.Simple (addRequestHeader, getResponseBody, httpBS, parseRequest)
import System.Directory (doesFileExist, getFileSize)
import System.Environment (getArgs, lookupEnv)

baseUrl :: String
baseUrl = "https://adventofcode.com/"

getRequest :: (MonadThrow m) => String -> Day -> m Request
getRequest token day = do
  req <- parseRequest route
  return $ addRequestHeader "cookie" (pack token) req
  where
    route = baseUrl <> show year <> "/day/" <> show day <> "/input"

fetchInput :: Day -> IO ()
fetchInput day = do
  isCached <- checkFileExistsWithData file
  token' <- lookupEnv "AOC_TOKEN"
  case (isCached, token') of
    (True, _) -> putStrLn "Input has been downloaded already."
    (False, Nothing) -> putStrLn "No session token."
    (False, Just token) -> do
      req <- getRequest token day
      response <- getResponseBody <$> httpBS req
      B.writeFile file response
  where
    file = getInputFile (F day)

checkFileExistsWithData :: FilePath -> IO Bool
checkFileExistsWithData fp = do
  exists <- doesFileExist fp
  if not exists
    then return False
    else do
      size <- getFileSize fp
      return $ size > 0

main :: IO ()
main = do
  loadFile defaultConfig
  [day] <- map read <$> getArgs
  fetchInput day
