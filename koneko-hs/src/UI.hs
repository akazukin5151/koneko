module UI (drawUI) where

import UI.GalleryUI ( galleryUI )
import UI.WelcomeUI ( welcomeUI )
import UI.PromptUI ( promptUI )
import UI.ArtistListUI ( artistListUI )
import UI.SingleImageUI ( singleImageUI )
import Types
    ( activeView,
      View(SingleImageView, GalleryView, WelcomeView, PromptView,
           ArtistListView), Field, St )
import Lens.Micro ((^.))
import Brick (Widget)

drawUI :: St -> [Widget Field]
drawUI st =
  case st^.activeView of
    GalleryView -> galleryUI st
    WelcomeView -> welcomeUI st
    PromptView -> promptUI st
    ArtistListView -> artistListUI st
    SingleImageView -> singleImageUI st
