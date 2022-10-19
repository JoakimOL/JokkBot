{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Config
    ( Config
    , readConfigFromFile
    , parse
    , configSeparator
    , keyValue
    , keyValueFile
    ) where

import System.IO
import qualified Text.Parsec           as Parsec
import qualified Data.Map              as M
import qualified Data.ByteString.Char8 as B
import Text.Parsec ((<?>))
import Control.Applicative

parse rule = Parsec.parse rule "(source)"

configSeparator :: Parsec.Parsec B.ByteString () ()
configSeparator = Parsec.spaces >> Parsec.char '=' >> Parsec.spaces

configIdentifier :: Parsec.Parsec B.ByteString() B.ByteString
configIdentifier = do
    c  <- Parsec.alphaNum
    cs <- Parsec.many (Parsec.alphaNum <|> Parsec.char '_')
    return (B.pack $ c:cs)

keyValue :: Parsec.Parsec B.ByteString () (B.ByteString, B.ByteString)
keyValue = do
    key <- configIdentifier
    configSeparator
    value <- configIdentifier
    return (key,value)

keyValueFile = Parsec.endBy keyValue (Parsec.oneOf "\n\r")

-- data Config = Config
--     { hostname :: String
--     , hostport :: Int
--     , username :: String
--     , clientId :: String
--     , clientSecret :: String
--     , code :: String
--     }

type Config = M.Map B.ByteString B.ByteString

readConfigFromFile :: FilePath -> IO (Maybe Config)
readConfigFromFile configPath = do
    contents <- B.readFile configPath
    case parse keyValueFile contents of
      -- Right config -> print config
      -- Left _ -> putStrLn "bbb"
      Right config -> return $ Just (M.fromList config)
      Left _ -> return Nothing

    -- return (Config "a" 1 "a" "a" "a" "a")
