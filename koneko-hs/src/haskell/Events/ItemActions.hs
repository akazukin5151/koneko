module Events.ItemActions where

import Types
    ( Event, St, selectedCellIdx, requestsCache1, currentPage1, Request (urls) )
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
import Lens.Micro ((^.), (&))
import Data.IntMap ((!))
import System.Process (callProcess)

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
openInBrowser st = do
--  let idx = st^.selectedCellIdx
--  let request = (st^.requestsCache1) ! (st^.currentPage1)
--  let image_id = (request !! idx) & userIllust_id
--  let url = "https://www.pixiv.net/artworks/" <> image_id
--  callProcess "xdg-open" [url]
  pure st

artistListViewEvent :: St -> BrickEvent n Event -> EventM n2 (Next St)
artistListViewEvent st e =
  commonEvent st e (navigableFallback artistListViewFallback)

artistListViewFallback st' (VtyEvent (V.EvKey (V.KChar 'i') [])) = continue st'
artistListViewFallback st' (VtyEvent (V.EvKey (V.KChar 'v') [])) = continue st'
artistListViewFallback st' _ = continue st'

postViewEvent :: St -> BrickEvent n1 Event -> EventM n2 (Next St)
postViewEvent st e =
  commonEvent st e (navigableFallback postViewFallback)

postViewFallback st' (VtyEvent (V.EvKey (V.KChar 'd') [])) = continue st'
postViewFallback st' (VtyEvent (V.EvKey (V.KChar 'o') [])) = continue st'
postViewFallback st' (VtyEvent (V.EvKey (V.KChar 'v') [])) = continue st'
postViewFallback st' (VtyEvent (V.EvKey (V.KChar 'f') [])) = continue st'
postViewFallback st' (VtyEvent (V.EvKey (V.KChar 'r') [])) = continue st'
postViewFallback st' _ = continue st'
