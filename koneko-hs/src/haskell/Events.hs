module Events where

import Types
    ( activeView, Event(..), Field, St, View(PromptView, WelcomeView) )
import Brick ( EventM, BrickEvent, Next )
import Events.Core
    ( back,
      handleEnterView,
      handleH,
      handleJ,
      handleK,
      handleL,
      handleN,
      handleP, handleLogin, prefetchInBg )
import Brick.Types ( BrickEvent(AppEvent, VtyEvent) )
import qualified Graphics.Vty as V
import Brick.Main ( continue, halt )
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Events.WelcomeEvent (welcomeEvent)
import Lens.Micro ((^.))
import Events.PromptEvent (promptEvent)

appEvent :: St -> BrickEvent n Event -> EventM Field (Next St)
appEvent st e =
  case st^.activeView of
    WelcomeView -> welcomeEvent st e
    PromptView -> promptEvent st e
    _ ->
      case e of
        VtyEvent (V.EvKey (V.KChar 'q') []) -> halt st
        VtyEvent (V.EvKey (V.KChar 'b') []) -> continue =<< liftIO (back st)
        VtyEvent (V.EvKey (V.KChar 'l') []) -> continue =<< liftIO (handleL st)
        VtyEvent (V.EvKey (V.KChar 'h') []) -> continue =<< liftIO (handleH st)
        VtyEvent (V.EvKey (V.KChar 'j') []) -> continue =<< liftIO (handleJ st)
        VtyEvent (V.EvKey (V.KChar 'k') []) -> continue =<< liftIO (handleK st)
        VtyEvent (V.EvKey (V.KChar 'n') []) -> continue =<< liftIO (handleN st)
        VtyEvent (V.EvKey (V.KChar 'p') []) -> continue =<< liftIO (handleP st)
        AppEvent (ModeEnter mode)           -> continue =<< liftIO (handleEnterView st mode)
        AppEvent (LoginResult e_i)          -> continue =<< liftIO (handleLogin st e_i)
        AppEvent (DownloadFinished new_st)  -> continue =<< liftIO (prefetchInBg new_st)
        AppEvent (RequestFinished new_st)   -> continue new_st
        _                                   -> continue st
