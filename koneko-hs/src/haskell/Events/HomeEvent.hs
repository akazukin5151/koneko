module Events.HomeEvent where

import Core ( modes, modeIdxtoMode, highlightedMode, strToInt )
import Common ( validInput, modeToView, updateFooter )
import Events.Common ( historyDown, historyUp, wrapped)
import Types
    ( activeView,
      chan,
      currentSlice,
      displayedImages,
      modeIdx,
      selectedCellIdx,
      Event(ModeEnter),
      Mode(ArtistIllustrations, PixivPost,
           SearchArtists),
      St,
      View(PromptView), isHistoryFocused, historyIdx, history, editor, ub )
import Brick ( continue, halt )
import qualified Graphics.Vty as V
import Lens.Micro ((^.), (.~), (&), (%~), ix, (^?))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Brick.Types ( EventM, BrickEvent(VtyEvent), Next )
import Brick.BChan ( writeBChan )
import Brick.Widgets.Edit (applyEdit)
import Data.Text.Zipper (textZipper)
import Data.Char (isDigit)
import Events.Core (commonEvent)
import Data.Text (unpack)
import Graphics.Ueberzug (clear)
import Core (continueL)

homeEvent :: St -> BrickEvent n1 Event -> EventM n2 (Next St)
homeEvent st e =
  commonEvent st e (\st e ->
    case e of
      VtyEvent (V.EvKey V.KEsc []) -> halt st
      VtyEvent (V.EvKey (V.KChar 'q') []) -> halt st
      VtyEvent (V.EvKey (V.KChar 'j') []) -> continue $ handleDown st
      VtyEvent (V.EvKey (V.KChar 'k') []) -> continue $ handleUp st
      VtyEvent (V.EvKey (V.KChar '\t') []) | isHistoryActive st ->
        continue $ st & isHistoryFocused %~ not
      VtyEvent (V.EvKey (V.KChar c) []) -> continue $ handleJump st c
      VtyEvent (V.EvKey V.KEnter []) -> handleEnter st
      _ -> continue st
   )

handleJump :: St -> Char -> St
handleJump st 'a' = st & modeIdx .~ 0
handleJump st 'i' = st & modeIdx .~ 1
handleJump st 'f' = st & modeIdx .~ 2
handleJump st 's' = st & modeIdx .~ 3
handleJump st 'n' = st & modeIdx .~ 4
handleJump st 'r' = st & modeIdx .~ 5
handleJump st d
  | isDigit d && digit >= 0 && digit <= (length modes - 1) = st & modeIdx .~ digit
  where
    digit = strToInt [d]
handleJump st _   = st

isHistoryActive :: St -> Bool
isHistoryActive st =
  case highlightedMode st of
    ArtistIllustrations -> True
    PixivPost -> True
    SearchArtists -> True
    _ -> False

handle :: (St -> p) -> (St -> p) -> St -> p
handle f g st =
  if st^.isHistoryFocused
     then f st
     else g st

handleUp :: St -> St
handleUp = handle historyUp radioUp

handleDown :: St -> St
handleDown = handle historyDown radioDown

radioDown :: St -> St
radioDown = radio (+1)

radioUp :: St -> St
radioUp = radio (subtract 1)

radio :: (Int -> Int) -> St -> St
radio f st = st & modeIdx %~ wrapped (length modes - 1) f

handleEnter :: St -> EventM n (Next St)
handleEnter = handle historySelect modeSelect

select :: St -> IO St
select st = do
  Right () <- clear (st^.ub) "home"
  pure $ st & displayedImages .~ []
            & isHistoryFocused .~ False
            & historyIdx .~ 0

historySelect :: St -> EventM n (Next St)
historySelect st = do
  case historyHighlighted ^? ix (st^.historyIdx) of
    Nothing -> continue st
    Just (_, input) -> do
      let new_mode = modeIdxtoMode (st ^. modeIdx)
      if validInput new_mode $ unpack input
         then do
           new_st <- liftIO $ select st
           let newer_st = new_st & editor %~ applyEdit (const $ textZipper [input] Nothing)
           continueL $ onSelectNoPrompt new_mode newer_st
         else continue st
  where
    historyHighlighted =
      filter (\(mode, _) -> mode == highlightedMode st) $ st^.history

modeSelect :: St -> EventM n (Next St)
modeSelect st = do
  new_st <- liftIO $ select st
  continueL $ onSelect (modeIdxtoMode (new_st ^. modeIdx)) new_st

onSelectNoPrompt :: Mode -> St -> IO St
onSelectNoPrompt m st = do
  let new_st = st & selectedCellIdx .~ 0
                  & activeView .~ modeToView m
                  & currentSlice .~ 0
                  & updateFooter
  writeBChan (new_st^.chan) (ModeEnter m)
  pure new_st

onSelectToPrompt :: St -> IO St
onSelectToPrompt st =
  pure $ st & activeView .~ PromptView
            & updateFooter

onSelect :: Mode -> St -> IO St
onSelect ArtistIllustrations st = onSelectToPrompt st
onSelect PixivPost st           = onSelectToPrompt st
onSelect SearchArtists st       = onSelectToPrompt st
onSelect m st                   = onSelectNoPrompt m st
