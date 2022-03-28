module Events where

import Types
    ( activeView, Event, Field, St, View(PromptView, WelcomeView) )
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
import Events.WelcomeEvent (welcomeEvent)
import Lens.Micro ((^.))
import Events.PromptEvent (promptEvent)

appEvent :: St -> BrickEvent n Event -> EventM Field (Next St)
appEvent st e =
  case st^.activeView of
    WelcomeView -> welcomeEvent st e
    PromptView -> promptEvent st e
    _ -> commonEvent st e (\st e ->
      case e of
        VtyEvent (V.EvKey (V.KChar 'q') []) -> halt st
        VtyEvent (V.EvKey (V.KChar 'b') []) -> continue =<< liftIO (back st)
        VtyEvent (V.EvKey (V.KChar 'l') []) -> continue =<< liftIO (handleL st)
        VtyEvent (V.EvKey (V.KChar 'h') []) -> continue =<< liftIO (handleH st)
        VtyEvent (V.EvKey (V.KChar 'j') []) -> continue =<< liftIO (handleJ st)
        VtyEvent (V.EvKey (V.KChar 'k') []) -> continue =<< liftIO (handleK st)
        VtyEvent (V.EvKey (V.KChar 'n') []) -> continue =<< liftIO (handleN st)
        VtyEvent (V.EvKey (V.KChar 'p') []) -> continue =<< liftIO (handleP st)
        _                                   -> continue st
      )
