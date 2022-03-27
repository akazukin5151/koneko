{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Download.Core where

import Common ( getEditorText)
import Types
    ( conn,
      images,
      labels,
      Mode(FollowingArtists, ArtistIllustrations, SingleIllustration,
           SearchArtists, FollowingArtistsIllustrations, RecommendedIllustrations),
      St, your_id )
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
  -> IO (Either String ([String], [FilePath], [String], Maybe String))
fetchFirst = fetch 0

fetch
  :: Int
  -> Mode
  -> St
  -> String
  -> IO (Either String ([String], [FilePath], [String], Maybe String))
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

downloadFromScratch :: Mode -> St -> String -> [FilePath] -> [String] -> IO St
downloadFromScratch mode st dir sorted urls =
  case mode of
    ArtistIllustrations ->
      downloadThenUpdate (downloadUserIllust st dir sorted urls) mode st
    SingleIllustration ->
      downloadThenUpdate (downloadIllustDetail st dir sorted urls) mode st
    SearchArtists ->
      downloadThenUpdate (downloadUserFollowing st sorted urls) mode st
    FollowingArtists ->
      downloadThenUpdate (downloadUserFollowing st sorted urls) mode st
    FollowingArtistsIllustrations ->
      downloadThenUpdate (downloadUserIllust st dir sorted urls) mode st
    RecommendedIllustrations ->
      downloadThenUpdate (downloadUserIllust st dir sorted urls) mode st

-- technically imgs and labels are already updated,
-- but need to update the ub instance anyway
downloadThenUpdate :: IO [String] -> Mode -> St -> IO St
downloadThenUpdate io_imgs mode st = do
  imgs <- io_imgs
  (labels', _) <- findImagesView mode st
  -- it's probably reversed because the first images to be downloaded
  -- will finish downloading first, and the last image in order
  -- will download last and thus be the first item of @imgs@
  let new_st' = st & images .~ sort imgs
  --new_st' <- showImagesView new_st new_st
  pure $ new_st' & labels .~ (pack <$> labels')

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
