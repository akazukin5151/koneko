module UI.Common where

import Brick
    ( Widget,
      on,
      fill,
      setAvailableSize,
      updateAttrMap,
      withBorderStyle, applyAttrMappings, fg )
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Border as B
import qualified Brick.AttrMap as A
import qualified Graphics.Vty.Attributes as V
import Types (isHistoryFocused, St)
import Brick.Widgets.Border (borderAttr)
import Graphics.Vty (blue)
import Lens.Micro ((^.))


mapEnumerate :: (Int -> b -> c) -> [b] -> [c]
mapEnumerate f xs = zipWith f [0 .. length xs] xs

mkEmptyCell :: (Int, Int) -> Widget n
mkEmptyCell size = mkCell size $ fill ' '

mkCell :: (Int, Int) -> Widget n -> Widget n
mkCell size =
  withBorderStyle BS.unicode .
  B.border .
  setAvailableSize size

mkCellWithSize :: (Int, Int) -> Widget n -> Widget n
mkCellWithSize size =
  withBorderStyle BS.unicode .
  B.border .
  setAvailableSize size

mkEmptyCellWithSize :: (Int, Int) -> Widget n
mkEmptyCellWithSize = flip mkCellWithSize $ fill ' '

highlight :: Widget n -> Widget n
highlight = updateAttrMap (A.applyAttrMappings [ (B.borderAttr, V.yellow `on` V.black) ])

colorFocused :: St -> Bool -> Widget n -> Widget n
colorFocused st c =
  if st^.isHistoryFocused == c
     then updateAttrMap (applyAttrMappings [(borderAttr, fg blue)])
     else id
