{-# LANGUAGE OverloadedStrings #-}

module Events.Core where

import Common ( coordsToIndex, indexToCoords,  viewToNRowsCols, updateFooter, nextOffset, logger)
import Core ( intToStr, highlightedMode)
import Types
    ( St,
      Event(ModeEnter, DownloadFinished, RequestFinished),
      Mode(Home),
      View(WelcomeView, SingleImageView),
      activeView,
      chan,
      currentSlice,
      displayedImages,
      editor,
      images,
      labels,
      selectedCellIdx,
      ub,
      currentPage1,
      your_id,
      footer,
      offset,
      pendingOnLogin, nextUrl )
import Graphics.Ueberzug ( clear )
import Control.Monad (void, unless)
import Lens.Micro ((^.), (.~), (&), (%~), (?~), (<&>))
import Data.Maybe (fromMaybe)
import System.FilePath (takeFileName, (</>))
import Brick.BChan (writeBChan)
import Brick.Widgets.Edit (applyEdit)
import Data.Text.Zipper (clearZipper)
import Events.ShowImages (showImagesView)
import Events.FindImages (findImagesView)
import Events.Common ( getDirectory, listDirectoryFullSorted, getFirstDirectory, wrapped, getNextDirectory)
import System.Directory (doesDirectoryExist, listDirectory)
import Download.Core (downloadFromScratch, fetchFirst, downloadWithoutShowing, fetch)
import Brick (txt)
import Data.Text (pack)
import Control.Arrow ((<<<), (>>>))
import Control.Concurrent (forkIO)
import System.Directory.Internal (andM)

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
handleSliceOrPage f cond sliceReset st =
  if cond
     then do
       clearImages st
       let new_st = st & currentSlice %~ f
                       & displayedImages .~ []
       showImagesView st new_st
     else do
       let next_dir = getDirectory st </> intToStr (f (st^.currentPage1))
       has_next <- doesDirectoryExist next_dir
       if has_next
         then do
           clearImages st
           new_images <- listDirectoryFullSorted next_dir
           -- TODO: next page -> set nextUrl to the next one (aka, set the "current"
           -- json to the next page, so that the next url points to the next-next)
           -- eg page 1 -> page 2
           -- page_1_response -> page_2_response
           -- page_1_response.next_url (page_2_url) -> page_2_response.next_url (page_3_url)
           let new_st =
                 st^.nextUrl
                 >>= (pack >>> nextOffset)
                 <&> (\o -> st & offset .~ o)
                 & fromMaybe st
                 & currentSlice .~ sliceReset
                 & currentPage1 %~ f
                 -- & offset %~ f
                 & displayedImages .~ []
                 & images .~ new_images
           logger "new_st^.nextUrl" $ new_st^.nextUrl
           showImagesView st new_st
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
  | st^.activeView == SingleImageView = length (st^.images) - 1
  | otherwise = length (st^.images) `div` (ncols*nrows) - 1
  where (nrows, ncols) = viewToNRowsCols st

back :: St -> IO St
back st = do
  mapM_ (clear (st^.ub)) (takeFileName <$> st^.displayedImages)
  writeBChan (st^.chan) (ModeEnter Home)
  pure $ st & editor %~ applyEdit clearZipper
            & activeView .~ WelcomeView
            & displayedImages .~ []
            & updateFooter

handleEnterView :: St -> Mode -> IO St
handleEnterView st mode = do
  let dir = getFirstDirectory st
  cond <- andM (doesDirectoryExist dir) (listDirectory dir <&> (not <<< null))
  if cond
    then do
      (labels', imgs) <- findImagesView mode st
      let new_st = st & images .~ imgs
      new_st' <- showImagesView new_st new_st
      -- TODO: verify cache up-to-date
      void $ forkIO $ do
        Right (labels', imgs, urls, m_nextUrl) <- fetchFirst mode new_st' dir
        let new_st'' =
              st & images .~ imgs
                 & labels .~ (pack <$> labels')
                 & nextUrl .~ m_nextUrl
        writeBChan (st^.chan) (RequestFinished new_st'')
        prefetchInner new_st'' mode
      pure $ new_st' & labels .~ (pack <$> labels')
    else
      case st^.your_id of
        Nothing -> pure $ st & pendingOnLogin ?~ downloadInBg mode dir
        Just _ -> downloadInBg mode dir st

prefetchInner :: St -> Mode -> IO ()
prefetchInner st mode = do
  let dir = getNextDirectory st
  cond <- andM (doesDirectoryExist dir) (listDirectory dir <&> (not <<< null))
  logger "st^.nextUrl" $ st^.nextUrl
  -- prefetch only if dir doesn't exist or is empty
  unless cond $
    void $ forkIO $ do
      case st^.nextUrl >>= (pack >>> nextOffset) of
        Nothing -> pure ()
        Just no -> do
          Right (_labels', imgs, urls, m_nextUrl) <- fetch no mode st dir
          -- TODO: do i need to store these in next_labels/imgs?
          let new_st = st & nextUrl .~ m_nextUrl
          writeBChan (st^.chan) (RequestFinished new_st)
          downloadWithoutShowing mode st dir imgs urls

prefetchInBg :: St -> IO St
prefetchInBg st = do
  let mode = highlightedMode st
  prefetchInner st mode
  pure st

downloadInBg :: Mode -> String -> St -> IO St
downloadInBg mode dir st = do
  void $ forkIO $ do
    Right (labels', imgs, urls, m_nextUrl) <- fetchFirst mode st dir
    let new_st =
          st & images .~ imgs
             & labels .~ (pack <$> labels')
             & nextUrl .~ m_nextUrl
    writeBChan (st^.chan) (RequestFinished new_st)
    new_st' <- downloadFromScratch mode new_st dir imgs urls
    writeBChan (st^.chan) (DownloadFinished new_st')
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
