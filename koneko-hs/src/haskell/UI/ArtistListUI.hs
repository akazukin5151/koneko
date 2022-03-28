module UI.ArtistListUI where

import qualified Brick.Widgets.Center as C
import Brick ( hBox, (<=>), vBox, Widget )
import Common ( indexToCoords, viewToNRowsCols )
import Types
    ( currentSlice, footer, selectedCellIdx, Field, St, config, labels_, currentPage1, request )
import Lens.Micro ( (^.), (&) )
import UI.Common ( highlight, mkCell, mkEmptyCell, mapEnumerate )
import Config.Types (boxWidth, boxHeight)
import Text.Wrap (WrapSettings(breakLongWords), defaultWrapSettings)
import Data.IntMap ((!))
import Brick.Widgets.Core (strWrapWith)

artistListUI :: St -> [Widget Field]
artistListUI st = [ui]
  where
    ui = C.center $ vBox vs <=> (st^.footer)
    (nrows, ncols) = viewToNRowsCols st
    labels = (st^.request) ! (st^.currentPage1) & labels_
    names = drop (st^.currentSlice * nrows) $ labels
    number_of_previews = 3
    size = (st^.config.boxWidth, st^.config.boxHeight)
    cell = mkEmptyCell size
    wrapper = strWrapWith (defaultWrapSettings {breakLongWords = True})
    hs name =
      [cell] <> [mkCell size (wrapper name)] <> replicate number_of_previews cell
    h_funcs = replicate nrows hs
    vs = mapEnumerate f [fn name | (name, fn) <- zip names h_funcs]
    f idx hbox | idx == py = hBox $ mapEnumerate g hbox
               | otherwise = hBox hbox
    g idx cell' | idx == px = highlight cell'
                | otherwise = cell'
    (px, py) = indexToCoords ncols (st^.selectedCellIdx)
