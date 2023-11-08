{-# LANGUAGE OverloadedStrings #-}

module Main where

import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Monad.Catch (MonadThrow)
import qualified Data.ByteString as B
import Data.ByteString.Char8 (pack)
import Network.HTTP.Conduit (Request)
import Network.HTTP.Simple (addRequestHeader, getResponseBody, httpBS, parseRequest)
import System.Directory (doesFileExist, getFileSize)
import System.Environment (getArgs, lookupEnv)
import Text.Printf (printf)

baseUrl :: String
baseUrl = "https://adventofcode.com/"

getRequest :: (MonadThrow m) => String -> Int -> Int -> m Request
getRequest token year day = do
  req <- parseRequest route
  return $ addRequestHeader "cookie" (pack token) req
  where
    route = baseUrl <> show year <> "/day/" <> show day <> "/input"

fetchInput :: Int -> Int -> FilePath -> IO ()
fetchInput year day dir = do
  isCached <- checkFileExistsWithData filepath
  token' <- lookupEnv "AOC_TOKEN"
  case (isCached, token') of
    (True, _) -> putStrLn "Input has been downloaded already."
    (False, Nothing) -> putStrLn "No session token."
    (False, Just token) -> do
      req <- getRequest token year day
      response <- getResponseBody <$> httpBS req
      B.writeFile filepath response
  where
    filepath = printf "%s/input%02d.txt" dir day

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
  fetchInput 2022 day "inputs/"
