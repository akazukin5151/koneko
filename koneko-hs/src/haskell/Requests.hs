{-# LANGUAGE LambdaCase #-}

module Requests where

import Serialization.Out
    ( PixivRequest(..),
      IPCActions(Login, Request, Download),
      RefreshToken,
      IPCJson(IPCJson, action, ident), Offset, Query, UserId, ImageId, DownloadInfo (..), Url )
import Sockets ( sendEither )
import Data.ByteString.Lazy.Char8 ( toStrict )
import Data.Aeson ( encode )
import Types (St, conn, messageQueue)
import Lens.Micro ((^.), (&), (<&>), (%~))
import Data.IntMap (lookupMax, insert, keys)
import Data.Maybe (fromMaybe)
import Serialization.In (IPCResponses)
import Data.Functor (($>))
import Common (logger)
import System.Directory (createDirectoryIfMissing)
import Control.Monad (zipWithM, foldM)
import Core (enumerate)
import Data.IntMap.Lazy (union)
import Data.Either (rights)

request :: PixivRequest -> St -> (IPCResponses -> IO ()) -> IO (Either String St)
request x st cb = do
  let i = st^.messageQueue & lookupMax <&> fst & fromMaybe 0 & (+ 1)
  let new_st = st & messageQueue %~ insert i cb
  let r = IPCJson {ident = i, action = Request x}
  ei <- sendEither st $ toStrict $ encode r
  pure $ ei $> new_st

download
  :: (Int -> IPCResponses -> IO ())
  -> St
  -> [Url]
  -> [FilePath]  -- ^ output paths
  -> [FilePath]  -- ^ output names
  -> IO (Either String St)
download cb st urls dirs names = do
  mapM_ (createDirectoryIfMissing True) dirs
  let i = st^.messageQueue & lookupMax <&> fst & fromMaybe 0 & (+ 1)
  logger "[i..(i + length infos)]" [i..(i + length infos)]
  let sts = [ do
        let new_st = st & messageQueue %~ insert i (cb idx)
        new_st
        | (idx, i) <- enumerate [i..(i + length infos)]]
  let r = IPCJson {ident = i, action = Download infos}
  ei <- sendEither st $ toStrict $ encode r
  let new_st = foldr1 f sts
  pure $ ei $> new_st
  -- pure $ Right $ foldr1 f $ rights sts
  where
    f a b = a & messageQueue %~ (`union` (b^.messageQueue))
    infos =
        [ DownloadInfo
           { url = url'
           , path = dir
           , name = name'
           }
        | (url', dir, name') <- zip3 urls dirs names]

login :: RefreshToken -> St -> (IPCResponses -> IO ()) -> IO (Either String St)
login x st cb = do
  let i = st^.messageQueue & lookupMax <&> fst & fromMaybe 0 & (+ 1)
  let new_st = st & messageQueue %~ insert i cb
  let r = IPCJson {ident = i, action = Login x}
  ei <- sendEither st $ toStrict $ encode r
  pure $ ei $> new_st

userIllustRequest
  :: UserId -> Offset -> St -> (IPCResponses -> IO ()) -> IO (Either String St)
userIllustRequest user_id offset = request (UserIllusts user_id offset)

illustDetailRequest
  :: ImageId -> St -> (IPCResponses -> IO ()) -> IO (Either String St)
illustDetailRequest image_id = request (IllustDetail image_id)

userFollowingRequest
  :: UserId -> Offset -> St -> (IPCResponses -> IO ()) -> IO (Either String St)
userFollowingRequest user_id offset = request (UserFollowing user_id offset)

searchUserRequest
  :: Query -> Offset -> St -> (IPCResponses -> IO ()) -> IO (Either String St)
searchUserRequest query offset = request (SearchUser query offset)

illustFollowRequest
  :: Offset -> St -> (IPCResponses -> IO ()) -> IO (Either String St)
illustFollowRequest offset = request (IllustFollow offset)

illustRecommendedRequest
  :: Offset -> St -> (IPCResponses -> IO ()) -> IO (Either String St)
illustRecommendedRequest offset = request (IllustRecommended offset)
