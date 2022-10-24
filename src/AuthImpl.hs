{-# LANGUAGE OverloadedStrings #-}
module AuthImpl
    ( openBrowser
    , getCode
    , getRequestParameters
    , getRequestParameterValue
    , runTCPServer
    ) where

import System.Exit(ExitCode(..))
import System.Process(rawSystem)
import qualified Data.ByteString as B

import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import Network.Socket
import Network.Socket.ByteString(recv)
import Control.Concurrent
import Control.Monad(unless,forever,void)
import qualified Control.Exception as E
import System.IO

openBrowser :: String -> IO Bool
openBrowser url = exitCodeToBool `fmap` rawSystem executable argv
    where (executable, argv) = ("sh", ["-c", "xdg-open \"$0\" 2>&1 > /dev/null", url])
          exitCodeToBool ExitSuccess     = True
          exitCodeToBool (ExitFailure _) = False

getCode :: IO B.ByteString
getCode = do
    response <- runTCPServer "3000"
    print
        $ getRequestParameterValue "code"
        $ getRequestParameters
        $ T.decodeUtf8 response
    return response

getRequestParameterValue :: T.Text -> [T.Text] -> Maybe T.Text
getRequestParameterValue text params = case hits of
                     [x, y]     -> Just y
                     -- TODO this is ugly
                     (x:y:z)    -> Just y
                     _otherwise -> Nothing
                   where hits = concat
                              . filter (\x -> head x == text)
                              $ map (T.splitOn "=") params

getRequestParameters :: T.Text -> [T.Text]
getRequestParameters text = concatMap (T.splitOn "&") (T.words text)

runTCPServer :: ServiceName -> IO B.ByteString
runTCPServer port = do
    addr <- resolve
    E.bracket (open addr) close connect
  where
    resolve = do
        let hints = defaultHints {
                addrFlags = [AI_PASSIVE]
              , addrSocketType = Stream
              }
        head <$> getAddrInfo (Just hints) Nothing (Just port)
    open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
        setSocketOption sock ReuseAddr 1
        withFdSocket sock setCloseOnExecIfNeeded
        bind sock $ addrAddress addr
        listen sock 1024
        return sock
    connect sock = E.bracket (accept sock) (close . fst) $ \(conn, _) -> recv conn 2048
