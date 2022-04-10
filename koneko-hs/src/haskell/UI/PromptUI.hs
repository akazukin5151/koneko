{-# LANGUAGE OverloadedStrings #-}

module UI.PromptUI where

import qualified Brick.Widgets.Edit as E
import Lens.Micro ((^.))
import Brick
    ( (<+>), withAttr, hLimitPercent, vBox, (<=>), vLimit, Widget, Location (Location) )
import Types
    ( editor,
      history,
      historyIdx,
      isHistoryFocused,
      Field,
      Mode(SearchArtists, ArtistIllustrations, PixivPost),
      St )
import Brick.Widgets.Core (txt, translateBy)
import Common ( validInput, getEditorText )
import Core ( highlightedMode )
import Brick.Widgets.Edit (getEditContents)
import Brick.Widgets.Center (hCenter)
import Brick.Widgets.Border (borderWithLabel)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Char (toLower)
import Data.Maybe (fromMaybe, listToMaybe)
import UI.Common ( colorFocused, mapEnumerate )
import Data.Text (unpack, pack, Text)
import qualified Data.Text as T

promptUI :: St -> [Widget Field]
promptUI st = [ui]
  where
    ui = translateBy (Location (0, 5)) $ ed <=> historySection

    ed =
      st^.editor
      & E.renderEditor (color . txt . T.unlines) (not $ st^.isHistoryFocused)
      & borderWithLabel (txt (modeToPromptQuestion $ highlightedMode st))
      & hLimitPercent 40
      & vLimit 3
      & colorFocused st False
      & hCenter

    historySection =
      st^.history
      & filter filterHistory
      <&> txt
      & mapEnumerate g
      & vBox
      -- & setAvailableSize (10, 10)
      & borderWithLabel (txt "History")
      & colorFocused st True
      & hCenter

    w = listToMaybe $ unpack <$> getEditContents (st^.editor)
    searchStr = pack $ toLower <$> fromMaybe "" w
    filterHistory item = searchStr `T.isInfixOf` T.toLower item

    color =
      if validInput (highlightedMode st) $ unpack $ getEditorText st
         then withAttr $ "input" <> "valid"
         else withAttr $ "input" <> "invalid"

    g idx widget | idx == st^.historyIdx && st^.isHistoryFocused = txt "> " <+> widget
                 | otherwise = txt "  " <+> widget

modeToPromptQuestion :: Mode -> Text
modeToPromptQuestion ArtistIllustrations = "Artist user ID"
modeToPromptQuestion PixivPost = "Image ID"
modeToPromptQuestion SearchArtists = "Search query"
