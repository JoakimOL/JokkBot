{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( connectToIrc
    , loop
    ) where

import Config

import Control.Monad
import Control.Concurrent
import System.IO
import System.Exit
import qualified Data.ByteString.Char8 as B
import qualified Data.Map              as M
import Data.Text.Encoding as TLE
import Data.Map((!))
import Data.Text
import Network.HTTP.Client
import Network.HTTP.Types.Status (Status(statusCode))
import Network.Simple.TCP
import Irc.Commands
import Irc.Identifier (idText)
import Irc.Message
import Irc.RawIrcMsg
import Irc.UserInfo (userNick, UserInfo)

data BotContext = BotContext
    { socket :: Socket
    , remoteAddr :: SockAddr
    }

crlf :: B.ByteString
crlf = "\r\n"

sendPong :: [Text] -> BotContext -> IO()
sendPong txt ctx = do
    print ("sending pong to " ++ show (remoteAddr ctx) ++ " " ++ show (socket ctx) ++ " with text: " ++ Prelude.concatMap unpack txt)
    -- send (socket ctx) "PONG :tmi.twitch.com" 
    send (socket ctx) $ renderRawIrcMsg $ ircPong txt
    print "sent pong"

sendPrivMsg :: BotContext -> String -> IO()
sendPrivMsg ctx msg =
    send (socket ctx) $ B.intercalate (TLE.encodeUtf8 $ Data.Text.pack msg) ["PRIVMSG #pikkintheface :", crlf]

handlePrivMsg :: Irc.UserInfo.UserInfo -> Text -> IO()
handlePrivMsg user text =
    case trimmed_text of
      "aaaa" -> print "I can dispatch a handler here"
      "bbbb" -> print "Found something else! I can dispatch another handler here"
      _uninteresting -> 
        do
            print "uninteresting message:"
            print user
            print trimmed_text
      where trimmed_text = strip text

passCommand :: BotContext-> IO()
passCommand ctx = do
    cmd <- fmap Prelude.words getLine
    case cmd of
      ("send" : params) ->
          do
              let msg = Prelude.unwords params
              sendPrivMsg ctx msg
      _unknown -> print "unknown command"
    return()

handleMessage :: B.ByteString -> BotContext -> IO ()
handleMessage text ctx = do
    case parseRawIrcMsg $ TLE.decodeUtf8 text of
      Just msg ->  do 
          let cookedMsg = cookIrcMsg msg
          case cookedMsg of 
            Ping txt -> do
                putStrLn "detected ping!"
                sendPong txt ctx
            Privmsg user _ text -> do
                putStrLn "detected privmsg!"
                handlePrivMsg user text
            _ -> do 
                putStrLn "unhandled message:"
                print text
      Nothing -> putStrLn " fak"

loop :: BotContext -> IO()
loop ctx = do
        msg <- recv (socket ctx) 512
        case msg of 
          Just text -> handleMessage text ctx
          Nothing -> die "connection to socket lost"
        loop ctx

connectToIrc :: Config.Config -> IO ()
connectToIrc config = do
    -- connect "localhost" "6667" $ \(socket, remoteAddr) -> do
    connect "irc.chat.twitch.tv" "6667" $ \(socket, remoteAddr) -> do
        let context = BotContext socket remoteAddr
        send socket $ B.intercalate (config ! "code") ["PASS oauth:", crlf]
        send socket $ B.intercalate (config ! "username") ["NICK ", crlf]
        send socket $ B.intercalate (config ! "channel") ["JOIN #", crlf]
        print socket
        print remoteAddr
        send socket "CAP REQ :twitch.tv/tags\r\n"
        -- send socket ("JOIN #pikkintheface" `B.append` crlf)
        -- send socket ("CAP REQ :twitch.tv/tags" `B.append` crlf)
        forkIO $ loop context
        forever $ passCommand context
    putStrLn "hei"
