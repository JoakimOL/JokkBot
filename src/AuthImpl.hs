{-# LANGUAGE OverloadedStrings #-}
module AuthImpl
    ( openBrowser
    , getCode
    , extractCode
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
    response <- runTCPServer Nothing "3000"
    (print . T.words . T.decodeUtf8) response
    print $ extractCode $ T.decodeUtf8 response
    return response

extractCode :: T.Text -> Maybe T.Text
extractCode text = case hits of
                     [x, y]     -> Just y
                     _otherwise -> Nothing
                   where hits = concat
                              . filter (\x -> length x == 2)
                              $ map (T.splitOn "code=") (T.words text)
-- extractCode text = (!! 1)                                 -- get the right hand side of the match [text] -> text
--                  . concat                                 -- flatten list [[text]] -> [text]
--                  . filter (\x -> length x == 2)           -- throw away words that didnt match
--                  $ map (T.splitOn "code=") (T.words text) -- split all words on "code=" [text] -> [[text]]

runTCPServer :: Maybe HostName -> ServiceName -> IO B.ByteString
runTCPServer mhost port = do
    addr <- resolve
    E.bracket (open addr) close connect
  where
    resolve = do
        let hints = defaultHints {
                addrFlags = [AI_PASSIVE]
              , addrSocketType = Stream
              }
        Prelude.head <$> getAddrInfo (Just hints) mhost (Just port)
    open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
        setSocketOption sock ReuseAddr 1
        withFdSocket sock setCloseOnExecIfNeeded
        bind sock $ addrAddress addr
        listen sock 1024
        return sock
    connect sock = E.bracket (accept sock) (close . fst) $ \(conn, _) -> recv conn 2048
