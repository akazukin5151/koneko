module Events.ShowImages where

import Common ( indexToCoords, viewToNRowsCols )
import Core ( enumerate )
import Types
    ( St,
      View(SingleImageView, GalleryView, ArtistListView),
      activeView,
      currentSlice,
      displayedImages,
      images,
      ub )
import Graphics.Ueberzug
    ( defaultUbConf,
      draw,
      Scalers(FitContain),
      UbConf(identifier, path, x, y, width, height, scaler),
      Ueberzug )
import Lens.Micro ((^.), (.~), (&))
import System.FilePath (takeFileName)

showImagesView :: St -> St -> IO St
showImagesView st =
  case st^.activeView of
    GalleryView -> showImagesSimple (showImageView st) id
    ArtistListView -> showImagesSimple (showImageView st) (subtract 1)
    SingleImageView -> showImagesSingle

showImageView :: St -> Ueberzug -> (Int, FilePath) -> IO ()
showImageView st =
  case st^.activeView of
    GalleryView ->
        showImageSimple 5 (\px -> 2 + px * 19) (\py -> 3 + py * 10)
    ArtistListView ->
        showImageSimple 4 (\px -> 5 + px * 20) (\py -> 2 + py * 12)
    SingleImageView -> showImageSingle'

showImageInner
  :: a
  -> b
  -> (a -> Int)
  -> (b -> Int)
  -> (a -> b -> Maybe Int)
  -> (a -> b -> Maybe Int)
  -> Ueberzug
  -> FilePath
  -> IO ()
showImageInner px py xposF yposF wF hF ub' filepath = do
  Right () <-
    draw ub' $ defaultUbConf
      { identifier = takeFileName filepath
      , path = filepath
      , x = xposF px
      , y = yposF py
      , width = wF px py
      , height = hF px py
      , scaler = Just FitContain
      }
  pure ()

showImageSimple
  :: Int
  -> (Int -> Int)
  -> (Int -> Int)
  -> Ueberzug
  -> (Int, FilePath)
  -> IO ()
showImageSimple ncols xposF yposF ub' (idx, filepath) =
  showImageInner px py xposF yposF (const2 $ Just 15) (const2 $ Just 15) ub' filepath
    where
      (px, py) = indexToCoords ncols idx
      const2 x _ _ = x

showImagesSimple
  :: (Ueberzug -> (Int, String) -> IO ()) -> (Int -> Int) -> St -> IO St
showImagesSimple f g st = do
   let (nrows, ncols_) = viewToNRowsCols st
   let ncols = g ncols_
   let c = st^.currentSlice
   let subset = take (ncols * nrows) $ drop (c * ncols * nrows) (st^.images)
   mapM_ (f (st^.ub)) (enumerate subset)
   pure $ st & displayedImages .~ (takeFileName <$> subset)

showImagesSingle :: St -> IO St
showImagesSingle st = do
  let c = st^.currentSlice
  case st^.images of
    [] -> pure st
    [main_img] -> do
      showImageSingle (st^.ub) (1, main_img)
      pure $ st & displayedImages .~ [takeFileName main_img]
    _ -> do
      let main_img = (st^.images) !! c
      let other_imgs = drop (c + 1) (st^.images)
      let subset = take 4 other_imgs
      case subset of
        (x : xs) -> do
          let i = [x] <> [main_img] <> xs
          mapM_ (showImageSingle (st^.ub)) (enumerate i)
          pure $ st & displayedImages .~ (takeFileName <$> i)
        [] -> do
          showImageSingle (st^.ub) (1, main_img)
          pure $ st & displayedImages .~ [takeFileName main_img]

-- todo replace this one with showImageSingle'
showImageSingle :: Ueberzug -> (Int, FilePath) -> IO ()
showImageSingle ub' (idx, filepath) = do
  showImageInner px py xposF yposF wF hF ub' filepath
    where
      tmp = indexToCoords 3 idx
      -- pretend last image is in the corner
      (px, py) = if tmp == (1, 1) then (2, 1) else tmp
      xposF px' = if (px', py) == (1, 0) then 34 else 2 + px' * 38
      yposF py' = 2 + py' * 11
      wF px' py' = if (px', py') == (1, 0) then Just 30 else Just 13
      hF px' py' = if (px', py') == (1, 0) then Just 19 else Just 13

-- showImageSingle uses this mental model:
-- (header=col number, index=row number, cell numbers = image idx)
--    | 0 | 1 | 2
-- ---|---|---|---
--  0 | 0 | 1 | 2
--  1 | 3 |   | 4
--          ^   ^
--          |->-|  shift 1 (pretend (1,1) is (2,1))
--
-- with coords:
-- [(0,0),(1,0),(2,0),(0,1),(2,1)]
--
-- This uses a different model
--    | 0 | 1 | 2
-- ---|---|---|---
--  0 | 1 | 0 | 2
--  1 | 3 |   | 4
--          ^   ^
--          |->-|  shift 1 (pretend (1,1) is (2,1))
--
-- with coords:
-- [(1,0), (0,0), (2,0), (0,1), (2,1)]
--   ^       ^
--   |--<--<-|  swap 1  (pretend (0,0) is second image)
--   |       |
--   |-->-->-|  swap 2  (pretend (1,0) is first image)
--
-- This guarantees that the first image is in the center, which is better
-- because it does not require knowledge of the total number of images
showImageSingle' :: Ueberzug -> (Int, FilePath) -> IO ()
showImageSingle' ub' (idx, filepath) = do
  showImageInner px py xposF yposF wF hF ub' filepath
    where
      tmp = indexToCoords 3 idx
      (px, py) =
        case tmp of
          (0, 0) -> (1, 0)  -- swap 1
          (1, 0) -> (0, 0)  -- swap 2
          (1, 1) -> (2, 1)  -- shift 1
          _ -> tmp
      xposF px' = if (px', py) == (1, 0) then 34 else 2 + px' * 38
      yposF py' = 2 + py' * 11
      wF px' py' = if (px', py') == (1, 0) then Just 30 else Just 13
      hF px' py' = if (px', py') == (1, 0) then Just 19 else Just 13
