module UI.SingleImageUI where

import Brick ( Widget, (<=>), hBox, vBox )
import UI.Common ( mkEmptyCellWithSize, highlight, mapEnumerate )
import Common ( indexToCoords, viewToNRowsCols )
import Types ( footer, selectedCellIdx, Field, St )
import Lens.Micro ( (^.) )

singleImageUI :: St -> [Widget Field]
singleImageUI st = [ui]
  where
    -- total_slices = length (st^.images) `div` (ncols*nrows) - 1
    ui = hBox test <=> (st^.footer)
    -- length boxes == _nrows
    boxes =
      [ [mkEmptyCellWithSize (17, 9), mkEmptyCellWithSize (17, 9)]
      , [mkEmptyCellWithSize (55, 20)]
      , [mkEmptyCellWithSize (17, 9), mkEmptyCellWithSize (17, 9)]
      ]
    test = mapEnumerate f boxes
    f idx hbox | idx == px = vBox $ mapEnumerate g hbox
               | otherwise = vBox hbox
    g idx cell | idx == py = highlight cell
               | otherwise = cell
    -- pretend the 2 vertical cells in the middle always have y-coord of 0
    -- eg When seeing (1,1), convert to (1,0)
    -- this is actually hardcoded to only work with 3 cols, because 1 is the middle
    -- column
    tmp = indexToCoords ncols (st^.selectedCellIdx)
    (px, py) = if tmp == (1, 1) then (1, 0) else tmp
    (_nrows, ncols) = viewToNRowsCols st
