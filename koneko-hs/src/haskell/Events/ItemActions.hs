{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Events.ItemActions where

import Types
    ( Event, St, selectedCellIdx, requestsCache1, currentPage1, Request (urls, image_ids, original_urls), currentSlice, editor, Mode (PixivPost) )
import Brick ( EventM, BrickEvent, Next )
import Events.Core
    ( back,
      handleH,
      handleJ,
      handleK,
      handleL,
      handleN,
      handleP, commonEvent, clearImages )
import Brick.Types ( BrickEvent(VtyEvent) )
import qualified Graphics.Vty as V
import Brick.Main ( continue, halt )
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Lens.Micro ((^.), (&), (^?), ix, (%~), (<&>), (.~))
import Data.IntMap ((!), empty)
import System.Process (callProcess)
import Core (intToStr, continueL)
import Common (viewToNRowsCols)
import Serialization.In (IllustDetail(illustDetail_meta_single_page, illustDetail_meta_pages), MetaSinglePage (original_image_url), MetaPage (image_urls), ImageUrl (original))
import Requests (download)
import Lens.Micro.Internal ( Ixed, Index, IxValue )
import System.Directory (getHomeDirectory)
import System.FilePath ((</>), (<.>))
import Data.Text (splitOn, unpack, pack)
import Brick.Widgets.Edit (applyEdit)
import Data.Text.Zipper (textZipper)
import Events.HomeEvent (onSelectNoPrompt)

navigableFallback :: (St -> BrickEvent n1 e -> EventM n2 (Next St)) -> St -> BrickEvent n1 e -> EventM n2 (Next St)
navigableFallback fallback st' e' =
  case e' of
    VtyEvent (V.EvKey (V.KChar 'q') []) -> halt st'
    VtyEvent (V.EvKey (V.KChar 'b') []) -> continueL $ back st'
    VtyEvent (V.EvKey (V.KChar 'l') []) -> continueL $ handleL st'
    VtyEvent (V.EvKey (V.KChar 'h') []) -> continueL $ handleH st'
    VtyEvent (V.EvKey (V.KChar 'j') []) -> continueL $ handleJ st'
    VtyEvent (V.EvKey (V.KChar 'k') []) -> continueL $ handleK st'
    VtyEvent (V.EvKey (V.KChar 'n') []) -> continueL $ handleN st'
    VtyEvent (V.EvKey (V.KChar 'p') []) -> continueL $ handleP st'
    _                                   -> fallback st' e'

galleryEvent :: St -> BrickEvent n1 Event -> EventM n2 (Next St)
galleryEvent st e =
  commonEvent st e (navigableFallback galleryFallback)

galleryFallback :: St -> BrickEvent n1 e -> EventM n2 (Next St)
galleryFallback st' (VtyEvent (V.EvKey (V.KChar 'd') [])) =
  continueL $ downloadImage st'
galleryFallback st' (VtyEvent (V.EvKey (V.KChar 'o') [])) =
  continueL $ openInBrowser st'
galleryFallback st' (VtyEvent (V.EvKey (V.KChar 'v') [])) =
  continueL $ viewPost st'
galleryFallback st' _ = continue st'

viewPost :: St -> IO St
viewPost st = do
  case m_image_id of
    Just (Just image_id) -> do
      let new_editor = const $ textZipper [pack $ intToStr image_id] Nothing
      clearImages st
      -- TODO: ideally keep the cache separated by modes
      let new_st = st & editor %~ applyEdit new_editor
                      & requestsCache1 .~ empty
                      & currentPage1 .~ 1
      onSelectNoPrompt PixivPost new_st
    _ -> pure st
  where
    m_image_id = getNthFromCachedRequest image_ids st

downloadImage :: St -> IO St
downloadImage st = do
  case (m_original_url, m_image_id) of
    (Just (Just url), Just (Just image_id)) -> do
      home <- getHomeDirectory
      e_st <- download cb st [url] [home </> "Downloads"] [intToStr image_id <.> "jpg"]
      case e_st of
        Right s -> pure s
        _ -> pure st
    _ -> pure st
  where
    -- TODO: report status in footer
    cb _ _ = pure ()
    m_original_url = getNthFromCachedRequest original_urls st
    m_image_id = getNthFromCachedRequest image_ids st

openInBrowser :: St -> IO St
openInBrowser st =
  case m_image_id of
    Just (Just image_id) -> do
      let url = "https://www.pixiv.net/artworks/" <> intToStr image_id
      callProcess "xdg-open" [url]
      pure st
    _ -> pure st
  where
    m_image_id = getNthFromCachedRequest image_ids st

getNthFromCachedRequest
  :: (Ixed s, Index s ~ Int)
  => (Request -> s)
  -> St
  -> Maybe (IxValue s)
getNthFromCachedRequest f st = f request ^? ix idx
  where
    (nrows, ncols) = viewToNRowsCols st
    imgs_in_slice = nrows * ncols
    offset = imgs_in_slice * st^.currentSlice
    idx = st^.selectedCellIdx + offset
    request = (st^.requestsCache1) ! (st^.currentPage1)

artistListViewEvent :: St -> BrickEvent n Event -> EventM n2 (Next St)
artistListViewEvent st e =
  commonEvent st e (navigableFallback artistListViewFallback)

-- i for image, a for artist
artistListViewFallback st' (VtyEvent (V.EvKey (V.KChar 'i') [])) = continue st'
artistListViewFallback st' (VtyEvent (V.EvKey (V.KChar 'a') [])) = continue st'
artistListViewFallback st' _ = continue st'

postViewEvent :: St -> BrickEvent n1 Event -> EventM n2 (Next St)
postViewEvent st e =
  commonEvent st e (navigableFallback postViewFallback)

postViewFallback :: St -> BrickEvent n1 e -> EventM n2 (Next St)
postViewFallback st' (VtyEvent (V.EvKey (V.KChar 'd') [])) =
  continueL $ downloadImageInPost st'
postViewFallback st' (VtyEvent (V.EvKey (V.KChar 'o') [])) =
  continueL $ openPostInBrowser st'
postViewFallback st' (VtyEvent (V.EvKey (V.KChar 'v') [])) = continue st'
postViewFallback st' (VtyEvent (V.EvKey (V.KChar 'f') [])) = continue st'
postViewFallback st' (VtyEvent (V.EvKey (V.KChar 'r') [])) = continue st'
postViewFallback st' _ = continue st'

downloadImageInPost :: St -> IO St
downloadImageInPost st =
  case m_original_url of
    Just (Just url) -> do
      home <- getHomeDirectory
      let name = unpack $ last $ splitOn "/" $ pack url
      e_st <- download cb st [url] [home </> "Downloads"] [name]
      case e_st of
        Right s -> pure s
        _ -> pure st
    _ -> pure st
  where
    -- TODO: report status in footer
    cb _ _ = pure ()
    offset = st^.currentSlice
    idx =
      case st^.selectedCellIdx of
        0 -> 1
        1 -> 0
        4 -> 0
        5 -> 4
        x -> x
    request = (st^.requestsCache1) ! (st^.currentPage1)
    m_original_url = original_urls request ^? ix (idx + offset)

openPostInBrowser :: St -> IO St
openPostInBrowser st =
  case m_image_id of
    Just (Just image_id) -> do
      let url = "https://www.pixiv.net/artworks/" <> intToStr image_id
      callProcess "xdg-open" [url]
      pure st
    _ -> pure st
  where
    request = (st^.requestsCache1) ! (st^.currentPage1)
    -- a post only has one valid image_id for every image
    m_image_id = (request & image_ids) ^? ix 0
