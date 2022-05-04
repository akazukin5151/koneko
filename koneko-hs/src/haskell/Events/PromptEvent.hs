{-# LANGUAGE OverloadedStrings #-}

module Events.PromptEvent where

import Common
    ( validInput, modeToView, getEditorText, updateFooter )
import Core ( highlightedMode, intToStr, continueL )
import Types
    ( activeView,
      chan,
      editor,
      modeIdx,
      Event(ModeEnter, LoginResult, ReturnToHome),
      selectedCellIdx,
      View(HomeView),
      currentSlice,
      konekoDir,
      isHistoryFocused,
      historyIdx,
      history,
      St, Field )
import Brick ( continue, BrickEvent(VtyEvent, AppEvent), handleEventLensed )
import qualified Graphics.Vty as V
import Lens.Micro ((^.), (.~), (&), (%~))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Brick.Widgets.Edit (handleEditorEvent, applyEdit)
import qualified Brick.Types as T
import Brick.BChan (writeBChan)
import System.FilePath ((</>))
import Data.Text.Zipper (textZipper)
import Events.Common (historyDown, historyUp)
import Events.Core (handleLogin)
import Data.Text (Text, unpack, pack)
import qualified Data.Text.IO as T

promptEvent :: St -> BrickEvent n Event -> T.EventM Field (T.Next St)
promptEvent st e =
  case e of
    VtyEvent (V.EvKey V.KEsc []) -> continueL $ back st
    VtyEvent (V.EvKey (V.KChar 'q') []) | st^.isHistoryFocused -> continueL $ back st
    VtyEvent (V.EvKey (V.KChar '\t') []) ->
      continue $ st & isHistoryFocused %~ not
    VtyEvent (V.EvKey (V.KChar 'j') []) | st^.isHistoryFocused ->
      continue $ historyDown st
    VtyEvent (V.EvKey (V.KChar 'k') []) | st^.isHistoryFocused ->
      continue $ historyUp st
    VtyEvent (V.EvKey V.KEnter []) | not (st^.isHistoryFocused) ->
      continueL $ onEnterNormal st
    VtyEvent (V.EvKey V.KEnter []) | st^.isHistoryFocused ->
      continueL $ onEnterHistory st
    T.VtyEvent ev | not (st^.isHistoryFocused) ->
      continue =<< handleEventLensed st editor handleEditorEvent ev
    AppEvent (LoginResult e_i)          -> continueL $ handleLogin st e_i
    _ -> continue st

back :: St -> IO St
back st = do
  writeBChan (st^.chan) (ReturnToHome)
  pure $ st & activeView .~ HomeView
            & isHistoryFocused .~ False
            & historyIdx .~ 0
            & updateFooter

onEnterInner :: Text -> St -> IO St
onEnterInner ans st = do
  let new_mode = highlightedMode st
  let new_view = modeToView new_mode
  if validInput new_mode $ unpack ans
     then do
       let new_st = st & activeView .~ new_view
                       & selectedCellIdx .~ 0
                       & currentSlice .~ 0
                       & isHistoryFocused .~ False
                       & historyIdx .~ 0
                       & updateFooter
       saveHistory (st^.konekoDir) (st^.modeIdx) ans
       writeBChan (new_st^.chan) (ModeEnter new_mode)
       pure new_st
     else pure st

onEnterNormal :: St -> IO St
onEnterNormal st = onEnterInner (getEditorText st) st

onEnterHistory :: St -> IO St
onEnterHistory st = onEnterInner ans new_st
  where
    new_st = st & editor %~ applyEdit (const (textZipper [ans] Nothing))
    ans = (st^.history) !! (st^.historyIdx)

saveHistory :: FilePath -> Int -> Text -> IO ()
saveHistory dir mi input = T.appendFile file text
  where
    file = dir </> "history_new"
    text = pack (intToStr mi) <> ":" <> input <> "\n"
