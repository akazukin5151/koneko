module Events.WelcomeEvent where

import Core ( modes, modeIdxtoMode, highlightedMode, strToInt )
import Common ( validInput, modeToView, updateFooter )
import Graphics ( onAppStart, radioInner )
import Events.Common ( clearThenUpdate, historyDown, historyUp, wrapped)
import Types
    ( activeView,
      chan,
      currentSlice,
      displayedImages,
      modeIdx,
      selectedCellIdx,
      Event(ModeEnter, LoginResult),
      Mode(Home, ArtistIllustrations, SingleIllustration,
           SearchArtists),
      St,
      View(PromptView), isHistoryFocused, historyIdx, history, editor )
import Brick ( continue, halt )
import qualified Graphics.Vty as V
import Lens.Micro ((^.), (.~), (&), (%~))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Brick.Types ( EventM, BrickEvent(AppEvent, VtyEvent), Next )
import Brick.BChan ( writeBChan )
import Brick.Widgets.Edit (applyEdit)
import Data.Text.Zipper (textZipper)
import Data.Char (isDigit)
import Events.Core (handleLogin)
import Data.Text (unpack)

welcomeEvent :: St -> BrickEvent n1 Event -> EventM n2 (Next St)
welcomeEvent st e =
  case e of
    VtyEvent (V.EvKey V.KEsc []) -> halt st
    VtyEvent (V.EvKey (V.KChar 'q') []) -> halt st
    VtyEvent (V.EvKey (V.KChar 'j') []) -> continue =<< liftIO (handleDown st)
    VtyEvent (V.EvKey (V.KChar 'k') []) -> continue =<< liftIO (handleUp st)
    VtyEvent (V.EvKey (V.KChar '\t') []) | isHistoryActive st ->
      continue $ st & isHistoryFocused %~ not
    VtyEvent (V.EvKey (V.KChar c) []) -> continue =<< liftIO (handleJump st c)
    VtyEvent (V.EvKey V.KEnter []) -> handleEnter st
    AppEvent (ModeEnter Home) -> continue =<< onAppStart st
    AppEvent (LoginResult e_i)          -> continue =<< liftIO (handleLogin st e_i)
    _ -> continue st

handleJump :: St -> Char -> IO St
handleJump st 'a' = radioInner $ st & modeIdx .~ 0
handleJump st 'i' = radioInner $ st & modeIdx .~ 1
handleJump st 'f' = radioInner $ st & modeIdx .~ 2
handleJump st 's' = radioInner $ st & modeIdx .~ 3
handleJump st 'n' = radioInner $ st & modeIdx .~ 4
handleJump st 'r' = radioInner $ st & modeIdx .~ 5
handleJump st d | isDigit d =
  if digit >= 0 && digit <= (length modes - 1)
     then radioInner $ st & modeIdx .~ digit
     else pure st
  where
    digit = strToInt [d]
handleJump st _   = pure st

isHistoryActive :: St -> Bool
isHistoryActive st =
  case highlightedMode st of
    ArtistIllustrations -> True
    SingleIllustration -> True
    SearchArtists -> True
    _ -> False

handle :: (St -> p) -> (St -> p) -> St -> p
handle f g st =
  if st^.isHistoryFocused
     then f st
     else g st

handleUp :: St -> IO St
handleUp = handle (pure . historyUp) radioUp

handleDown :: St -> IO St
handleDown = handle (pure . historyDown) radioDown

radioDown :: St -> IO St
radioDown = radio (\st -> st^.modeIdx == (length modes - 1)) (+1)

radioUp :: St -> IO St
radioUp = radio (\st -> st^.modeIdx == 0) (subtract 1)

radio :: (St -> Bool) -> (Int -> Int) -> St -> IO St
radio cond _ st | cond st = pure st
radio _ f st = do
  let new_st = st & modeIdx %~ wrapped (length modes) f
  radioInner new_st

handleEnter :: St -> EventM n (Next St)
handleEnter = handle historySelect modeSelect

select :: MonadIO m => St -> m St
select st = do
  new_st' <- liftIO $ clearThenUpdate st "welcome"
  pure $ new_st' & displayedImages .~ []
                 & isHistoryFocused .~ False
                 & historyIdx .~ 0

historySelect :: St -> EventM n (Next St)
historySelect st = do
  let input = (st^.history) !! (st^.historyIdx)
  let new_mode = modeIdxtoMode (st ^. modeIdx)
  if validInput new_mode $ unpack input
     then do
       new_st <- select st
       let newer_st = new_st & editor %~ applyEdit (const $ textZipper [input] Nothing)
       onSelectNoPrompt new_mode newer_st
     else continue st

modeSelect :: St -> EventM n (Next St)
modeSelect st = do
  new_st <- select st
  onSelect (modeIdxtoMode (new_st ^. modeIdx)) new_st

onSelectNoPrompt :: Mode -> St -> EventM n (Next St)
onSelectNoPrompt m st = do
  let new_st = st & selectedCellIdx .~ 0
                  & activeView .~ modeToView m
                  & currentSlice .~ 0
                  & updateFooter
  liftIO $ writeBChan (new_st^.chan) (ModeEnter m)
  continue new_st

onSelectToPrompt :: St -> EventM n (Next St)
onSelectToPrompt st =
  continue $ st & activeView .~ PromptView
                & updateFooter

onSelect :: Mode -> St -> EventM n (Next St)
onSelect ArtistIllustrations st = onSelectToPrompt st
onSelect SingleIllustration st  = onSelectToPrompt st
onSelect SearchArtists st       = onSelectToPrompt st
onSelect m st                   = onSelectNoPrompt m st