{-# LANGUAGE OverloadedStrings #-}

module Events.Core where

import Common ( coordsToIndex, indexToCoords,  viewToNRowsCols, updateFooter, nextOffset, logger)
import Core ( intToStr, highlightedMode)
import Types
    ( St,
      Request(nextUrl_, paths, urls),
      Event(..),
      Mode(RecommendedIllustrations, Home, ArtistIllustrations,
           SingleIllustration, SearchArtists, FollowingArtists,
           FollowingArtistsIllustrations),
      View(WelcomeView, SingleImageView),
      activeView,
      chan,
      currentPage1,
      currentSlice,
      displayedImages,
      editor,
      footer,
      messageQueue,
      pendingOnLogin,
      requestsCache1,
      selectedCellIdx,
      ub,
      your_id )
import Graphics.Ueberzug ( clear )
import Control.Monad (void)
import Lens.Micro ((^.), (.~), (&), (%~), (?~), (<&>))
import Data.Maybe (fromMaybe)
import System.FilePath (takeFileName, (</>))
import Brick.BChan (writeBChan)
import Brick.Widgets.Edit (applyEdit)
import Data.Text.Zipper (clearZipper)
import Events.ShowImages (showImagesView, showImageView)
import Events.FindImages (findImagesView)
import Events.Common ( getDirectory, listDirectoryFullSorted, getFirstDirectory, wrapped, getNextDirectory)
import System.Directory (doesDirectoryExist, listDirectory)
import Download.Core (fetchFirst, downloadByMode, fetchWithPrefetchCb, requestCallback)
import Brick (txt, BrickEvent (AppEvent), continue)
import Data.Text (pack)
import Control.Arrow ((<<<), (>>>))
import Control.Concurrent (forkIO)
import System.Directory.Internal (andM)
import Data.IntMap (singleton, (!))
import Serialization.In (IPCResponses(Downloaded), IPCResponse (ident, IPCResponse, response))
import Download.Parsers
    ( parseIllustDetailResponse,
      parseUserDetailResponse,
      parseUserIllustResponse )
import Control.Monad.IO.Class (MonadIO(..))
import qualified Data.IntMap as M
import Brick.Types (EventM, Next)

commonEvent
  :: St
  -> BrickEvent n1 Event
  -> (St -> BrickEvent n1 Event -> EventM n2 (Next St))
  -> Brick.Types.EventM n2 (Next St)
commonEvent st e fallback =
  case e of
    AppEvent (ModeEnter mode)           -> continue =<< liftIO (handleEnterView st mode)
    AppEvent (LoginResult e_i)          -> continue =<< liftIO (handleLogin st e_i)
    AppEvent (UpdateSt new_st)   -> continue new_st
    AppEvent (IPCReceived r)            -> continue =<< liftIO (handle st r)
    _                                   -> fallback st e

handle :: St -> IPCResponse -> IO St
handle st IPCResponse {ident = i, response = r} = do
  let f = st^.messageQueue & M.lookup i & fromMaybe (const $ pure ())
  f r
  pure st

handleInner :: (Int -> Int) -> St -> IO St
handleInner f st = do
  let (nrows, ncols) = viewToNRowsCols st
  pure $ st & selectedCellIdx %~ wrapped (nrows* ncols - 1) f

handleL :: St -> IO St
handleL = handleInner (+1)

handleH :: St -> IO St
handleH = handleInner (subtract 1)

handleOnYAxis :: (Int -> Int) -> St -> IO St
handleOnYAxis f st = handleInner g st
  where
    g idx = do
      let (nrows, ncols) = viewToNRowsCols st
      let (px, py) = indexToCoords ncols idx
      fromMaybe idx $ coordsToIndex ncols nrows px $ f py

handleJ :: St -> IO St
handleJ = handleOnYAxis (+1)

handleK :: St -> IO St
handleK = handleOnYAxis (subtract 1)

clearImages :: St -> IO ()
clearImages st =
   mapM_ (clear (st^.ub)) $ st^.displayedImages

handleSliceOrPage :: (Int -> Int) -> Bool -> Int -> St -> IO St
handleSliceOrPage f isSamePage sliceReset st = do
  if isSamePage
     then do
       clearImages st
       let new_st = st & currentSlice %~ f
                       & displayedImages .~ []
       let images = (st^.requestsCache1) ! (st^.currentPage1) & paths
       showImagesView st new_st images
     else do
       let next_dir = getDirectory st </> intToStr (f (st^.currentPage1))
       has_next <- doesDirectoryExist next_dir
       if has_next
         then do
           clearImages st
           new_images <- listDirectoryFullSorted next_dir
           let new_st =
                 st
                 & currentSlice .~ sliceReset
                 & currentPage1 %~ f
                 -- & offset %~ f
                 & displayedImages .~ []
           showImagesView st new_st new_images
           prefetchInBg new_st
         else pure st

handleP :: St -> IO St
handleP st =
  handleSliceOrPage (subtract 1) (st^.currentSlice > 0) (totalSlices st) st

handleN :: St -> IO St
handleN st = handleSliceOrPage (+1) (st^.currentSlice < totalSlices st) 0 st

-- currentSlice is 0-indexed so need to subtract 1
totalSlices :: St -> Int
totalSlices st
  | st^.activeView == SingleImageView = length images - 1
  | otherwise = length images `div` (ncols*nrows) - 1
  where
    (nrows, ncols) = viewToNRowsCols st
    images = (st^.requestsCache1) ! (st^.currentPage1) & paths

back :: St -> IO St
back st = do
  mapM_ (clear (st^.ub)) (takeFileName <$> st^.displayedImages)
  writeBChan (st^.chan) (ModeEnter Home)
  pure $ st & editor %~ applyEdit clearZipper
            & activeView .~ WelcomeView
            & displayedImages .~ []
            & updateFooter

handleAction :: St -> Mode -> p -> Request -> IO ()
handleAction st' mode _ r' = do
  writeBChan (st'^.chan) (UpdateSt new_st')
  prefetchInner new_st' mode
    where
      new_st' = st' & requestsCache1 .~ singleton 1 r'

handleEnterView :: St -> Mode -> IO St
handleEnterView st mode = do
  let dir = getFirstDirectory st
  cond <- andM (doesDirectoryExist dir) (listDirectory dir <&> (not <<< null))
  if cond
    then do
      (labels', imgs) <- findImagesView mode st
      new_st <- showImagesView st st imgs
      -- TODO: verify cache up-to-date
      void $ forkIO $ do
        let cb =
              case mode of
                ArtistIllustrations -> requestCallback parseUserIllustResponse handleAction
                SingleIllustration -> requestCallback parseIllustDetailResponse handleAction
                SearchArtists -> requestCallback parseUserDetailResponse handleAction
                FollowingArtists -> requestCallback parseUserDetailResponse handleAction
                FollowingArtistsIllustrations -> requestCallback parseUserIllustResponse handleAction
                RecommendedIllustrations -> requestCallback parseUserIllustResponse handleAction
        Right new_st' <- fetchFirst cb mode new_st dir
        writeBChan (st^.chan) (UpdateSt new_st')
      pure new_st
    else do
      let func st' = downloadFromScratch mode dir st'
      case st^.your_id of
        Nothing -> pure $ st & pendingOnLogin ?~ func
        Just _ -> func st

prefetchInner :: St -> Mode -> IO ()
prefetchInner st mode = do
  let dir = getNextDirectory st
  void $ forkIO $ do
    let d = (st^.requestsCache1) ! (st^.currentPage1)
    case d & nextUrl_ >>= (pack >>> nextOffset) of
      Nothing -> pure ()
      Just no -> do
        r' <- fetchWithPrefetchCb no mode st dir
        case r' of
          Left _ -> pure ()
          Right new_st ->
            writeBChan (st^.chan) (UpdateSt new_st)

prefetchInBg :: St -> IO St
prefetchInBg st = do
  let mode = highlightedMode st
  prefetchInner st mode
  pure st

downloadAction :: St -> Mode -> [Char] -> Request -> IO ()
downloadAction st' mode dir r' = do
    let new_st = st' & requestsCache1 .~ singleton 1 r'
    writeBChan (st'^.chan) (UpdateSt new_st)
    e_new_st <- downloadByMode cb mode new_st dir (paths r') (urls r')
    case e_new_st of
      Right x -> do
        writeBChan (st'^.chan) (UpdateSt x)
        prefetchInner x mode
      _ -> pure ()
    where
      cb :: Int -> IPCResponses -> IO ()
      cb i x =
          case x of
              Downloaded s -> showImageView st' (st'^.ub) (i, s)
              _ -> pure ()

downloadFromScratch :: Mode -> String -> St -> IO St
downloadFromScratch mode dir st = do
  void $ forkIO $ do
    let cb =
          case mode of
            ArtistIllustrations -> requestCallback parseUserIllustResponse downloadAction
            SingleIllustration -> requestCallback parseIllustDetailResponse downloadAction
            SearchArtists -> requestCallback parseUserDetailResponse downloadAction
            FollowingArtists -> requestCallback parseUserDetailResponse downloadAction
            FollowingArtistsIllustrations -> requestCallback parseUserIllustResponse downloadAction
            RecommendedIllustrations -> requestCallback parseUserIllustResponse downloadAction
    Right s <- fetchFirst cb mode st dir
    writeBChan (st^.chan) (UpdateSt s)
  pure st

handleLogin :: St -> Either a String -> IO St
handleLogin st (Left e) = pure $ st & footer .~ txt "Login failed"
handleLogin st (Right i) = do
  new_st <-
    case st^.pendingOnLogin of
      Just f -> f st
      Nothing -> pure st
  pure $ new_st & your_id ?~ i
                & footer .~ txt "Login succeed"
                & pendingOnLogin .~ Nothing
