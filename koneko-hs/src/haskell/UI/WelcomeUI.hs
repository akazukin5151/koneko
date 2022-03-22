{-# LANGUAGE OverloadedStrings #-}

module UI.WelcomeUI where

import Brick
    ( Widget,
      (<+>),
      padTopBottom,
      translateBy,
      txt,
      vBox,
      vLimit,
      withAttr,
      Location(Location) )
import Core ( modes, enumerate, intToStr )
import Types
    ( footer,
      history,
      historyIdx,
      isHistoryFocused,
      modeIdx,
      Field,
      Mode(..),
      St )
import Lens.Micro ( (^.), (<&>) )
import Brick.Widgets.Border (borderWithLabel)
import Data.Function ((&))
import UI.Common (colorFocused, mapEnumerate)
import Brick.Widgets.Center (hCenter)
import Data.Text (Text, pack)

welcomeUI :: St -> [Widget Field]
welcomeUI st = [ui]
  where
    ui =
      translateBy (Location (2, 0)) $
        vBox $ [a] <> [historyText | st^.history /= []] <> [hCenter $ st^.footer]

    historyText =
      st^.history
      <&> txt
      & mapEnumerate g
      & vBox
      & borderWithLabel (txt "History")
      & colorFocused st True
      & vLimit 9
      & hCenter

    a =
      vBox
      [ padTopBottom 1 $ hCenter $ txt "koneko こねこ v1.0 (GPLv3)"
      , hCenter $ colorFocused st False $
        borderWithLabel (txt "Select an action") $
          vBox (mapEnumerate f radio)
      ]

    mode_str = fmt <$> modes
    radio = [txt (" " <> pack (intToStr idx) <> ". ") <+> z | (idx, z) <- enumerate mode_str]

    selectedRadio True  = txt "[*]"
    selectedRadio False = txt "[ ]"

    f idx widget | idx == st^.modeIdx = selectedRadio True <+> widget
                 | otherwise = selectedRadio False <+> widget

    g idx widget | idx == st^.historyIdx && st^.isHistoryFocused = txt "> " <+> widget
                 | otherwise = txt "  " <+> widget

fmt :: Mode -> Widget n
fmt ArtistIllustrations           = easyBold "" "a" "rtist illustrations"
fmt SingleIllustration            = easyBold "s" "i" "ngle illustration"
fmt FollowingArtists              = easyBold "" "f" "ollowing artists"
fmt SearchArtists                 = easyBold "" "s" "earch artists"
fmt FollowingArtistsIllustrations = easyBold "followi" "n" "g artists illustrations"
fmt RecommendedIllustrations      = easyBold "" "r" "ecommended illustrations"
fmt Info                          = txt "Info"
fmt Manual                        = txt "Manual"
fmt BrowseCache                   = txt "Browse cache (offline)"
fmt Home                          = txt "This should not happen"

easyBold :: Text -> Text -> Text -> Widget n
easyBold before letter after =
  txt before <+> withAttr "shortcut" (txt letter) <+> txt after
