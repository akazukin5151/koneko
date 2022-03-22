{-# LANGUAGE OverloadedStrings #-}

module Sockets where

import Lens.Micro ( (&), (<&>) )
import Network.Socket (Socket)
import Data.ByteString.Char8 (ByteString)
import Control.Exception (try, SomeException)
import Network.Socket.ByteString (sendAll, recv)
import Control.Arrow ((>>>))
import Data.Aeson (eitherDecodeStrict, encode)
import Serialization.In (IPCResponse (response), IPCResponses (ReportLen))
import qualified Data.ByteString as B
import Common (bindWithMsg)
import Serialization.Out (IPCJson(action, IPCJson))
import Data.ByteString.Lazy.Char8 (toStrict)
import qualified Serialization.Out as Out
import Control.Concurrent (threadDelay)

catchSomeException :: IO a -> IO (Either SomeException a)
catchSomeException = try

talk :: Socket -> ByteString -> IO (Either String ByteString)
talk conn text = do
  r <- catchSomeException (sendAll' conn text)
  case r of
    Left e -> pure $ Left $ show e
    Right _ -> recvAll conn

decodeReportLen :: IPCResponses -> Either String Int
decodeReportLen (ReportLen len) = Right len
decodeReportLen _ = Left "Wrong response type, expected ReportLen"

recvAll :: Socket -> IO (Either String ByteString)
recvAll conn = do
  header_bs <- recv conn 4096
  if header_bs == ""
     then recvAll conn
     else
      eitherDecodeStrict header_bs
        & bindWithMsg (response >>> decodeReportLen)
            "Decoding ReportLen inside IPCResponse failed: "
        <&> go ""
        & sequenceA

  where
    go :: ByteString -> Int -> IO ByteString
    go res total_len = do
      piece <- recv conn 4096
      let new = res <> piece
      let new_len = B.length new
      if new_len == total_len
         then pure new
         else go new total_len

sendAll' :: Socket -> ByteString -> IO ()
sendAll' conn text = do
  let x = Out.ReportLen (B.length text)
  let len = toStrict $ encode (IPCJson {action = x})
  _ <- catchSomeException (sendAll conn len)
  threadDelay 100000
  sendAll conn text
