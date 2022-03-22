{-# LANGUAGE LambdaCase #-}

module Requests where

import Network.Socket ( Socket )
import Serialization.Out
    ( PixivRequest(..),
      IPCActions(Login, Request),
      RefreshToken,
      IPCJson(IPCJson, action) )
import Serialization.In
    ( IPCResponse(response),
      IPCResponses(Requested, LoginInfo),
      IllustDetailResponse,
      LoginResponse,
      UserDetailResponse,
      UserIllustResponse )
import Common ( bindWithMsg )
import Sockets ( talk )
import Data.ByteString.Lazy.Char8 ( toStrict )
import Data.Aeson ( eitherDecode, eitherDecodeStrict, encode )
import Codec.Binary.UTF8.Generic (fromString)
import Data.Aeson.Types (FromJSON)
import Control.Arrow ((>>>))
import Data.Bifunctor (Bifunctor(first))
import Data.Function ((&))

request :: FromJSON b => PixivRequest -> Socket -> IO (Either String b)
request x conn = do
  res <- talk conn $ toStrict $ encode (IPCJson {action = Request x})
  pure $
    res
    & first ("Talking to socket failed: " <>)
    & bindWithMsg eitherDecodeStrict "Decoding request (IPCResponse) failed: "
    & bindWithMsg (response >>> decodeRequested)
        "Decoding Requested inside IPCResponse failed: "

login :: RefreshToken -> Socket -> IO (Either String LoginResponse)
login x conn = do
  res <- talk conn $ toStrict $ encode (IPCJson {action = Login x})
  pure $
    res
    & first ("Talking to socket failed: " <>)
    & bindWithMsg eitherDecodeStrict "Decoding request (IPCResponse) failed: "
    & bindWithMsg (response >>> decodeLogin)
        "Decoding LoginInfo inside IPCResponse failed: "

decodeLogin :: IPCResponses -> Either String LoginResponse
decodeLogin = \case
  LoginInfo json -> Right json
  _ -> Left "Wrong response type"

decodeRequested :: FromJSON b => IPCResponses -> Either String b
decodeRequested = \case
  Requested json -> eitherDecode (fromString json)
  _ -> Left "Wrong response type"

userIllustRequest :: String -> Int -> Socket -> IO (Either String UserIllustResponse)
userIllustRequest user_id offset = request (UserIllusts user_id offset)

illustDetailRequest :: String -> Socket -> IO (Either String IllustDetailResponse)
illustDetailRequest image_id = request (IllustDetail image_id)

userFollowingRequest :: String -> Int -> Socket -> IO (Either String UserDetailResponse)
userFollowingRequest user_id offset = request (UserFollowing user_id offset)

searchUserRequest :: String -> Int -> Socket -> IO (Either String UserDetailResponse)
searchUserRequest query offset = request (SearchUser query offset)

illustFollowRequest :: Int -> Socket -> IO (Either String UserIllustResponse)
illustFollowRequest offset = request (IllustFollow offset)

illustRecommendedRequest :: Int -> Socket -> IO (Either String UserIllustResponse)
illustRecommendedRequest offset = request (IllustRecommended offset)
