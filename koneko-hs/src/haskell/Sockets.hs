{-# LANGUAGE OverloadedStrings #-}

module Sockets where

import Network.Socket (Socket)
import Data.ByteString.Char8 (ByteString, split)
import Control.Exception (try, SomeException)
import Network.Socket.ByteString (sendAll, recv)
import Data.Aeson (eitherDecodeStrict, encode)
import Serialization.In (IPCResponses (ReportLen), IPCResponse (response))
import qualified Data.ByteString as B
import Serialization.Out (IPCJson(action, IPCJson, ident))
import Data.ByteString.Lazy.Char8 (toStrict)
import qualified Serialization.Out as Out
import Control.Concurrent (threadDelay)
import Data.Either (rights)
import Brick.BChan (writeBChan, BChan)
import Types (Event(IPCReceived), messageQueue, St (St), conn)
import Data.Foldable (foldl')
import Data.IntMap (lookupMax)
import Data.Functor ((<&>))
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import Lens.Micro ((^.))
import Codec.Binary.UTF8.Generic (fromString)

catchSomeException :: IO a -> IO (Either SomeException a)
catchSomeException = try

sendEither :: St -> ByteString -> IO (Either String ())
sendEither st text = do
  r <- catchSomeException (sendAllWithLen st text)
  case r of
    Left e -> pure $ Left $ show e
    Right _ -> pure $ Right ()

recvAll :: BChan Event -> Socket -> IO ()
recvAll chan conn = do
  msg_bs <- recv conn 4096
  if msg_bs == ""
     then recvAll chan conn
     else do
       let messages = split '\n' msg_bs
       let actions = map eitherDecodeStrict $ filter (/= "") messages
       mapM_ (handle chan conn) $ rights actions
       recvAll chan conn

handle :: BChan Event -> Socket -> IPCResponse -> IO ()
handle chan conn x = do
  case response x of
    (ReportLen l) -> do
      bs <- handleLen conn "" l
      case eitherDecodeStrict bs of
        Left _ -> pure ()
        Right d -> handle chan conn d
    _ -> do
      writeBChan chan $ IPCReceived x

handleLen :: Socket -> ByteString -> Int -> IO ByteString
handleLen conn res total_len = do
  piece <- recv conn 4096
  let messages = split '\n' piece
  let filtered = filter (/= "") messages
  let new = foldl' (<>) res filtered
  let new_len = B.length new
  if new_len == total_len
     then pure new
     else handleLen conn new total_len

sendAllWithLen :: St -> ByteString -> IO ()
sendAllWithLen st text = do
  let msg = text <> "\n"
  let x = Out.ReportLen (B.length msg)
  let i = st^.messageQueue & lookupMax <&> fst & fromMaybe 0 & (+ 1)
  let len_msg = toStrict $ encode (IPCJson {ident = i, action = x}) <> "\n"
  sendAll (st^.conn) len_msg
  threadDelay 100000
  sendAll (st^.conn) msg
