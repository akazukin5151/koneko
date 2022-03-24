module UI.GalleryUI where

import qualified Brick.Widgets.Center as C
import Brick ( Widget, (<=>), hBox, vBox )
import Common ( indexToCoords, viewToNRowsCols )
import Types ( footer, selectedCellIdx, Field, St )
import Lens.Micro ( (^.) )
import UI.Common ( highlight, mkEmptyCell, mapEnumerate )

galleryUI :: St -> [Widget Field]
galleryUI st = [a]
  where
    a = C.center $ vBox vs <=> (st^.footer)
    -- total_slices = length (st^.images) `div` (ncols*nrows) - 1
    vs = mapEnumerate f $ replicate nrows $ replicate ncols mkEmptyCell
      where
        f idx hbox | idx == py = hBox $ mapEnumerate g hbox
                   | otherwise = hBox hbox
        g idx cell | idx == px = highlight cell
                   | otherwise = cell
        (px, py) = indexToCoords ncols (st^.selectedCellIdx)
    (nrows, ncols) = viewToNRowsCols st
