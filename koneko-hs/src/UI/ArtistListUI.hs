module UI.ArtistListUI where

import qualified Brick.Widgets.Center as C
import Brick ( hBox, (<=>), txt, vBox, Widget )
import Common ( indexToCoords, viewToNRowsCols )
import Types
    ( currentSlice, footer, labels, selectedCellIdx, Field, St )
import Lens.Micro ( (^.) )
import UI.Common ( highlight, mkCell, mkEmptyCell, mapEnumerate )

artistListUI :: St -> [Widget Field]
artistListUI st = [ui]
  where
    ui = C.center $ vBox vs <=> (st^.footer)
    (nrows, ncols) = viewToNRowsCols st
    names = drop (st^.currentSlice * nrows) $ st^.labels
    number_of_previews = 3
    hs name =
      [mkEmptyCell] <> [mkCell (txt name)] <> replicate number_of_previews mkEmptyCell
    h_funcs = replicate nrows hs
    vs = mapEnumerate f [fn name | (name, fn) <- zip names h_funcs]
    f idx hbox | idx == py = hBox $ mapEnumerate g hbox
               | otherwise = hBox hbox
    g idx cell | idx == px = highlight cell
               | otherwise = cell
    (px, py) = indexToCoords ncols (st^.selectedCellIdx)
