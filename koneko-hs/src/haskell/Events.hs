module Events where

import Types
    ( activeView, Event, Field, St, View(PromptView, WelcomeView, GalleryView, ArtistListView), View(PostView) )
import Brick ( EventM, BrickEvent, Next )
import Events.WelcomeEvent (welcomeEvent)
import Lens.Micro ((^.))
import Events.PromptEvent (promptEvent)
import Events.ItemActions
    ( galleryEvent, artistListViewEvent, postViewEvent )

appEvent :: St -> BrickEvent n Event -> EventM Field (Next St)
appEvent st e =
  case st^.activeView of
    WelcomeView -> welcomeEvent st e
    PromptView -> promptEvent st e
    GalleryView -> galleryEvent st e
    ArtistListView -> artistListViewEvent st e
    PostView -> postViewEvent st e
