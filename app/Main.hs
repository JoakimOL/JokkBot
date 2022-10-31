module Main where

import Data.Aeson
import Data.Aeson.Parser
import qualified Data.ByteString.Lazy as B
import Options.Applicative
import System.Exit

import Lib
import Config


getJSON :: FilePath -> IO B.ByteString
getJSON = B.readFile
import Args

main :: IO ()
main = do
    cmdlineargs <- execParser parserInfo
    print cmdlineargs

    case (optFilename cmdlineargs) of
      Just s -> print s
      Nothing -> print "optFilename argument not set"

    result <- readConfigFromFile "config"
    case result of
      Just config -> do
        print config
        connectToIrc config
      Nothing ->
          die "Invalid configuration. Not able to parse"
    putStrLn "woow"
