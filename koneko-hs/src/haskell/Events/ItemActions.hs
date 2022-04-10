module Events.ItemActions where

import Types
    ( Event, St, selectedCellIdx, requestsCache1, currentPage1, Request (urls, image_ids), currentSlice )
import Brick ( EventM, BrickEvent, Next )
import Events.Core
    ( back,
      handleH,
      handleJ,
      handleK,
      handleL,
      handleN,
      handleP, commonEvent )
import Brick.Types ( BrickEvent(VtyEvent) )
import qualified Graphics.Vty as V
import Brick.Main ( continue, halt )
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Lens.Micro ((^.), (&), (^?), ix)
import Data.IntMap ((!))
import System.Process (callProcess)
import Core (intToStr)
import Common (viewToNRowsCols)

navigableFallback :: (St -> BrickEvent n1 e -> EventM n2 (Next St)) -> St -> BrickEvent n1 e -> EventM n2 (Next St)
navigableFallback fallback st' e' =
  case e' of
    VtyEvent (V.EvKey (V.KChar 'q') []) -> halt st'
    VtyEvent (V.EvKey (V.KChar 'b') []) -> continue =<< liftIO (back st')
    VtyEvent (V.EvKey (V.KChar 'l') []) -> continue =<< liftIO (handleL st')
    VtyEvent (V.EvKey (V.KChar 'h') []) -> continue =<< liftIO (handleH st')
    VtyEvent (V.EvKey (V.KChar 'j') []) -> continue =<< liftIO (handleJ st')
    VtyEvent (V.EvKey (V.KChar 'k') []) -> continue =<< liftIO (handleK st')
    VtyEvent (V.EvKey (V.KChar 'n') []) -> continue =<< liftIO (handleN st')
    VtyEvent (V.EvKey (V.KChar 'p') []) -> continue =<< liftIO (handleP st')
    _                                   -> fallback st' e'

galleryEvent :: St -> BrickEvent n1 Event -> EventM n2 (Next St)
galleryEvent st e =
  commonEvent st e (navigableFallback galleryFallback)

galleryFallback :: St -> BrickEvent n1 e -> EventM n2 (Next St)
galleryFallback st' (VtyEvent (V.EvKey (V.KChar 'd') [])) = continue st'
galleryFallback st' (VtyEvent (V.EvKey (V.KChar 'o') [])) =
  continue =<< liftIO (openInBrowser st')
galleryFallback st' (VtyEvent (V.EvKey (V.KChar 'v') [])) = continue st'
galleryFallback st' _ = continue st'

openInBrowser :: St -> IO St
openInBrowser st =
  case m_image_id of
    Just (Just image_id) -> do
      let url = "https://www.pixiv.net/artworks/" <> intToStr image_id
      callProcess "xdg-open" [url]
      pure st
    _ -> pure st
  where
    (nrows, ncols) = viewToNRowsCols st
    imgs_in_slice = nrows * ncols
    offset = imgs_in_slice * st^.currentSlice
    idx = st^.selectedCellIdx + offset
    request = (st^.requestsCache1) ! (st^.currentPage1)
    m_image_id = (request & image_ids) ^? ix idx

artistListViewEvent :: St -> BrickEvent n Event -> EventM n2 (Next St)
artistListViewEvent st e =
  commonEvent st e (navigableFallback artistListViewFallback)

artistListViewFallback st' (VtyEvent (V.EvKey (V.KChar 'i') [])) = continue st'
artistListViewFallback st' (VtyEvent (V.EvKey (V.KChar 'v') [])) = continue st'
artistListViewFallback st' _ = continue st'

postViewEvent :: St -> BrickEvent n1 Event -> EventM n2 (Next St)
postViewEvent st e =
  commonEvent st e (navigableFallback postViewFallback)

postViewFallback :: St -> BrickEvent n1 e -> EventM n2 (Next St)
postViewFallback st' (VtyEvent (V.EvKey (V.KChar 'd') [])) = continue st'
postViewFallback st' (VtyEvent (V.EvKey (V.KChar 'o') [])) =
  continue =<< liftIO (openPostInBrowser st')
postViewFallback st' (VtyEvent (V.EvKey (V.KChar 'v') [])) = continue st'
postViewFallback st' (VtyEvent (V.EvKey (V.KChar 'f') [])) = continue st'
postViewFallback st' (VtyEvent (V.EvKey (V.KChar 'r') [])) = continue st'
postViewFallback st' _ = continue st'

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
