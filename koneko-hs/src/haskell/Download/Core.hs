{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Download.Core where

import Common ( getEditorText)
import Types
    ( conn,
      Mode(FollowingArtists, ArtistIllustrations, SingleIllustration,
           SearchArtists, FollowingArtistsIllustrations, RecommendedIllustrations),
      St, your_id, Request )
import Lens.Micro ((^.), (.~), (&))
import Events.FindImages (findImagesView)
import Requests (userIllustRequest, illustDetailRequest, searchUserRequest, userFollowingRequest, illustFollowRequest, illustRecommendedRequest )
import Data.List ( sort )
import Data.Text ( pack, unpack )
import Data.Maybe (fromJust)
import Download.Parsers
    ( parseUserIllustResponse,
      parseIllustDetailResponse,
      parseUserDetailResponse )
import Download.Downloaders
    ( downloadUserIllust, downloadIllustDetail, downloadUserFollowing, downloadUserIllust', downloadIllustDetail', downloadUserFollowing' )
import Control.Monad (void)

fetchFirst
  :: Mode
  -> St
  -> String
  -> IO (Either String Request)
fetchFirst = fetch 0

fetch
  :: Int
  -> Mode
  -> St
  -> String
  -> IO (Either String Request)
fetch offset mode st dir =
  case mode of
    ArtistIllustrations -> do
      let artist_id = unpack $ getEditorText st
      r <- userIllustRequest artist_id offset (st^.conn)
      pure $ parseUserIllustResponse dir <$> r
    SingleIllustration -> do
      let image_id = unpack $ getEditorText st
      r <- illustDetailRequest image_id (st^.conn)
      pure $ parseIllustDetailResponse dir <$> r
    SearchArtists -> do
      let searchstr = unpack $ getEditorText st
      r <- searchUserRequest searchstr offset (st^.conn)
      pure $ parseUserDetailResponse dir <$> r
    FollowingArtists -> do
      r <- userFollowingRequest (fromJust $ st^.your_id) offset (st^.conn)
      pure $ parseUserDetailResponse dir <$> r
    FollowingArtistsIllustrations -> do
      r <- illustFollowRequest offset (st^.conn)
      pure $ parseUserIllustResponse dir <$> r
    RecommendedIllustrations -> do
      r <- illustRecommendedRequest offset (st^.conn)
      pure $ parseUserIllustResponse dir <$> r

downloadWithoutShowing :: Mode -> St -> String -> [FilePath] -> [String] -> IO ()
downloadWithoutShowing mode st dir sorted urls =
  case mode of
    ArtistIllustrations ->
      void (downloadUserIllust' st dir sorted urls)
    SingleIllustration ->
      void (downloadIllustDetail' st dir sorted urls)
    SearchArtists ->
      void (downloadUserFollowing' st sorted urls)
    FollowingArtists ->
      void (downloadUserFollowing' st sorted urls)
    FollowingArtistsIllustrations ->
      void (downloadUserIllust' st dir sorted urls)
    RecommendedIllustrations ->
      void (downloadUserIllust' st dir sorted urls)
