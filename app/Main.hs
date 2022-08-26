module Main where

import Data.Aeson
import Data.Aeson.Parser
import qualified Data.ByteString.Lazy as B
import System.Exit

import Lib
import Config


getJSON :: FilePath -> IO B.ByteString
getJSON = B.readFile

main :: IO ()
main = do
    result <- readConfigFromFile "config"
    case result of
      Just config -> do
        print config
        connectToIrc config
      Nothing -> 
          die "Invalid configuration. Not able to parse"
    putStrLn "woow"
