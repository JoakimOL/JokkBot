{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( connectToIrc
    , loop
    ) where

import Config
import Commands (lookUpStaticCommand)

import Control.Monad
import Control.Concurrent
import System.IO
import System.Exit
import qualified Data.ByteString.Char8 as B
import qualified Data.Map              as M
import Data.Text.Encoding as TLE
import Data.Map((!))
import Data.Text
import Data.Text.IO
import Text.Printf
import Network.Simple.TCP
import Irc.Commands
import Irc.Identifier (idText)
import Irc.Message
import Irc.RawIrcMsg
import Irc.UserInfo (userNick, UserInfo)

data BotContext = BotContext
    { socket :: !Socket
    , remoteAddr :: !SockAddr
    }

crlf :: B.ByteString
crlf = "\r\n"

sendPong :: [Text] -> BotContext -> IO()
sendPong txt ctx = do
    printf "sendng pong to %s with text: %s\n" (show (remoteAddr ctx)) (Prelude.concatMap unpack txt)
    send (socket ctx) $ B.append (TLE.encodeUtf8 "PONG :tmi.twitch.com") crlf
    print "sent pong"

sendPrivMsg :: BotContext -> Text -> IO()
sendPrivMsg ctx msg =
    send (socket ctx) $ B.intercalate (TLE.encodeUtf8 msg) [TLE.encodeUtf8 "PRIVMSG #pikkintheface :", crlf]

handlePrivMsg :: BotContext -> Irc.UserInfo.UserInfo -> Text -> IO()
handlePrivMsg ctx user text =
    case lookUpStaticCommand trimmed_text of
        Just response -> do 
           print "found something i know, sending auto response"
           sendPrivMsg ctx response
        Nothing -> print "yea i dunno"
      where trimmed_text = strip text

passCommand :: BotContext-> IO()
passCommand ctx = do
    cmd <- fmap Data.Text.words Data.Text.IO.getLine
    case cmd of
      ("send" : params) ->
          do
              let msg = Data.Text.unwords params
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
                print "detected ping!"
                sendPong txt ctx
            Privmsg source _ text -> do
                print "detected privmsg!"
                handlePrivMsg ctx (srcUser source) text
            _unhandled -> do
                printf "unhandled message: %s\n" (show text)
      Nothing -> print " fak"

loop :: BotContext -> IO()
loop ctx = do
    msg <- recv (socket ctx) 512
    case msg of
      Just text -> handleMessage text ctx
      Nothing -> die "connection to socket lost"
    loop ctx

--
-- remember, this get request gives you the auth token
-- https://id.twitch.tv/oauth2/authorize?response_type=token&client_id=<client_id>&redirect_uri=<redirect_url that i registered with>&scope=chat%3Aread+chat%3Aedit
--
connectToIrc :: Config.Config -> IO ()
connectToIrc config = do
    -- connect "localhost" "6667" $ \(socket, remoteAddr) -> do
    connect "irc.chat.twitch.tv" "6667" $ \(socket, remoteAddr) -> do
        let context = BotContext socket remoteAddr
        send socket $ B.intercalate (config ! "code") ["PASS oauth:", crlf]
        send socket $ B.intercalate (config ! "username") ["NICK ", crlf]
        send socket $ B.intercalate (config ! "channel") ["JOIN #", crlf]
        print socket
        print remoteAddr
        send socket "CAP REQ :twitch.tv/tags\r\n"
        -- send socket ("JOIN #pikkintheface" `B.append` crlf)
        -- send socket ("CAP REQ :twitch.tv/tags" `B.append` crlf)
        forkIO $ loop context
        forever $ passCommand context
