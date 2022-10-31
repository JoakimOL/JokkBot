module Args
    ( parserInfo
    , CmdLineOptions(..)
    ) where

import Options.Applicative
import Data.Semigroup((<>))

data CmdLineOptions = CmdLineOptions
    { optFilename             :: !(Maybe String)
    , optOverrideChannel      :: !(Maybe String)
    , optOverrideClientId     :: !(Maybe String)
    , optOverrideClientSecret :: !(Maybe String)
    , optOverrideToken        :: !(Maybe String) }
    deriving (Show)

filename :: Parser String
filename = strOption
    ( long "filename"
    <> short 'f'
    <> metavar "FILENAME"
    <> help "Path that points to a config file" )

overrideChannel :: Parser String
overrideChannel = strOption
    ( long "overrideChannel"
    <> short 'c'
    <> metavar "CHANNEL"
    <> help "Override the configurations value for channel" )

overrideClientId :: Parser String
overrideClientId = strOption
    ( long "overrideClientId"
    <> short 'i'
    <> metavar "CLIENTID"
    <> help "Override the configurations value for clientID" )

overrideClientSecret :: Parser String
overrideClientSecret = strOption
    ( long "overrideClientSecret"
    <> short 's'
    <> metavar "CLIENTSECRET"
    <> help "Override the configurations value for clientSecret" )

overrideToken :: Parser String
overrideToken = strOption
    ( long "overrideToken"
    <> short 't'
    <> metavar "TOKEN"
    <> help "Override the configurations value for the auth token" )


opts :: Parser CmdLineOptions
opts = CmdLineOptions <$> optional filename
                      <*> optional overrideChannel
                      <*> optional overrideClientId
                      <*> optional overrideClientSecret
                      <*> optional overrideToken

parserInfo :: ParserInfo CmdLineOptions
parserInfo = info (opts <**> helper)
    ( fullDesc
    <> progDesc "twitch bot and stuff"
    <> header "foo" )
