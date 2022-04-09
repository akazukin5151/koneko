
{-# LANGUAGE OverloadedStrings #-}

module Download.Core where

import Common ( getEditorText)
import Types
    ( Mode(FollowingArtists, ArtistIllustrations, SingleIllustration,
           SearchArtists, FollowingArtistsIllustrations, RecommendedIllustrations),
      St, your_id, Request (urls, paths), requestsCache1, Event (RequestFinished), chan, currentPage1 )
import Lens.Micro ((^.), (&), (%~), (<&>))
import Requests (userIllustRequest, illustDetailRequest, searchUserRequest, userFollowingRequest, illustFollowRequest, illustRecommendedRequest )
import Data.Text ( unpack )
import Data.Maybe (fromJust)
import Download.Parsers
    ( parseUserIllustResponse,
      parseIllustDetailResponse,
      parseUserDetailResponse )
import Download.Downloaders
    ( downloadUserIllust', downloadIllustDetail', downloadUserFollowing' )
import Control.Monad (unless)
import Serialization.In ( IPCResponses(Requested) )
import Data.Aeson (eitherDecodeStrict)
import Codec.Binary.UTF8.Generic (fromString)
import Brick.BChan (writeBChan)
import Data.IntMap (insert)
import System.Directory (doesDirectoryExist, listDirectory)
import Control.Arrow ((<<<))
import System.Directory.Internal (andM)
import Data.Aeson.Types (FromJSON)
import Serialization.Out (Offset, Url)

fetchFirst
  :: (Mode -> St -> a -> IPCResponses -> IO ())
  -> Mode -> St -> a -> IO (Either String St)
fetchFirst cb = fetch cb 0

fetchWithPrefetchCb :: Int -> Mode -> St -> String -> IO (Either String St)
fetchWithPrefetchCb offset mode = fetch cb offset mode
  where
    cb =
      case mode of
        ArtistIllustrations -> requestCallback parseUserIllustResponse prefetchAction
        SingleIllustration -> requestCallback parseIllustDetailResponse prefetchAction
        SearchArtists -> requestCallback parseUserDetailResponse prefetchAction
        FollowingArtists -> requestCallback parseUserDetailResponse prefetchAction
        FollowingArtistsIllustrations -> requestCallback parseUserIllustResponse prefetchAction
        RecommendedIllustrations -> requestCallback parseUserIllustResponse prefetchAction

requestCallback
  :: FromJSON a
  => (FilePath -> a -> Request)
  -> (St -> Mode -> FilePath -> Request -> IO ())
  -> Mode
  -> St
  -> FilePath
  -> IPCResponses
  -> IO ()
requestCallback parser action mode st dir ir =
  case ir of
    Requested s -> do
      let r = eitherDecodeStrict $ fromString s
      let e_req = fmap (parser dir) r
      case e_req of
        Left _ -> pure ()
        Right r' -> action st mode dir r'
    _ -> pure ()

prefetchAction :: St -> Mode -> FilePath -> Request -> IO ()
prefetchAction st mode dir r' = do
  let idx = st^.currentPage1 + 1
  let new_st = st & requestsCache1 %~ insert idx r'
  writeBChan (st^.chan) (RequestFinished new_st)
  -- download only if dir doesn't exist or is empty
  cond <-
    andM (doesDirectoryExist dir) (listDirectory dir <&> (not <<< null))
  unless cond $ do
    let cb _ _ = pure ()
    m_new_st <- downloadWithoutShowing cb mode st dir (paths r') (urls r')
    case m_new_st of
      Right x -> writeBChan (st^.chan) (RequestFinished x)
      _ -> pure ()


fetch
  :: (Mode -> St -> a -> IPCResponses -> IO ())
  -> Offset
  -> Mode
  -> St
  -> a
  -> IO (Either String St)
fetch cb offset mode st dir =
  case mode of
    ArtistIllustrations -> do
      let artist_id = unpack $ getEditorText st
      userIllustRequest artist_id offset st $
        cb mode st dir
    SingleIllustration -> do
      let image_id = unpack $ getEditorText st
      illustDetailRequest image_id st $
        cb mode st dir
    SearchArtists -> do
      let searchstr = unpack $ getEditorText st
      searchUserRequest searchstr offset st $
        cb mode st dir
    FollowingArtists ->
      userFollowingRequest (fromJust $ st^.your_id) offset st $
        cb mode st dir
    FollowingArtistsIllustrations ->
      illustFollowRequest offset st $
        cb mode st dir
    RecommendedIllustrations ->
      illustRecommendedRequest offset st $
        cb mode st dir

downloadWithoutShowing :: (Int -> IPCResponses -> IO ())
  -> Mode
  -> St
  -> String
  -> [FilePath]
  -> [Url]
  -> IO (Either String St)
downloadWithoutShowing cb mode st dir sorted urls =
  case mode of
    ArtistIllustrations ->
      downloadUserIllust' cb st dir sorted urls
    SingleIllustration ->
      downloadIllustDetail' cb st dir sorted urls
    SearchArtists ->
      downloadUserFollowing' cb st sorted urls
    FollowingArtists ->
      downloadUserFollowing' cb st sorted urls
    FollowingArtistsIllustrations ->
      downloadUserIllust' cb st dir sorted urls
    RecommendedIllustrations ->
      downloadUserIllust' cb st dir sorted urls
