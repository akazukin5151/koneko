module UI (drawUI) where

import UI.GalleryUI ( galleryUI )
import UI.HomeUI ( homeUI )
import UI.PromptUI ( promptUI )
import UI.ArtistListUI ( artistListUI )
import UI.SingleImageUI ( singleImageUI )
import Types
    ( activeView,
      View(PostView, GalleryView, HomeView, PromptView,
           ArtistListView), Field, St )
import Lens.Micro ((^.))
import Brick (Widget)

drawUI :: St -> [Widget Field]
drawUI st =
  case st^.activeView of
    GalleryView -> galleryUI st
    HomeView -> homeUI st
    PromptView -> promptUI st
    ArtistListView -> artistListUI st
    PostView -> singleImageUI st
