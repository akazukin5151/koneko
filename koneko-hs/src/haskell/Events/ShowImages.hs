module Events.ShowImages where

import Common ( indexToCoords, viewToNRowsCols )
import Core ( enumerate )
import Config.Types
import Types
import Graphics.Ueberzug
    ( defaultUbConf,
      draw,
      Scalers(FitContain),
      UbConf(identifier, path, x, y, width, height, scaler),
      Ueberzug )
import Lens.Micro ((^.), (.~), (&), (^?), ix)
import System.FilePath (takeFileName)
import Control.Monad (when)

showImagesView :: St -> St -> [FilePath] -> IO St
showImagesView st =
  case st^.activeView of
    GalleryView -> showImagesSimple (showImageView st) id
    ArtistListView -> showImagesSimple (showImageView st) (subtract 1)
    PostView -> showImagesSingle
    _ -> \_ _ -> pure st

-- TODO: home and prompt doesn't need this

showImageView :: St -> Ueberzug -> (Int, FilePath) -> IO ()
showImageView st =
  case st^.activeView of
    GalleryView -> do
      let xs = st^.config.galleryView & galleryViewConfig_image_xcoords
      let ys = st^.config.galleryView & galleryViewConfig_image_ycoords
      showImageSimple st (st^.config.ncols) xs ys
    ArtistListView -> do
      let xs = st^.config.artistListView & artistListViewConfig_image_xcoords
      let ys = st^.config.artistListView & artistListViewConfig_image_ycoords
      showImageSimple st 4 xs ys
    PostView -> showImageSingle'
    _ -> \_ _ -> pure ()

showImageInner
  :: a
  -> b
  -> Int
  -> Int
  -> (a -> b -> Maybe Int)
  -> (a -> b -> Maybe Int)
  -> Ueberzug
  -> FilePath
  -> IO ()
showImageInner px py xpos ypos wF hF ub' filepath = do
  Right () <-
    draw ub' $ defaultUbConf
      { identifier = takeFileName filepath
      , path = filepath
      , x = xpos
      , y = ypos
      , width = wF px py
      , height = hF px py
      , scaler = Just FitContain
      }
  pure ()

showImageSimple
  :: St
  -> Int
  -> [Int]
  -> [Int]
  -> Ueberzug
  -> (Int, FilePath)
  -> IO ()
showImageSimple st ncols' xs ys ub' (idx, filepath) =
  case (m_xpos, m_ypos) of
    (Just xp, Just yp) -> showImageInner px py xp yp w h ub' filepath
    _ -> pure ()
    where
      m_xpos = xs ^? ix px
      m_ypos = ys ^? ix py
      (px, py) = indexToCoords ncols' idx
      w = const2 (Just $ st^.config.imageWidth)
      h = const2 (Just $ st^.config.imageHeight)
      const2 a _ _ = a

showImagesSimple
  :: (Ueberzug -> (Int, FilePath) -> IO b)
  -> (Int -> Int)
  -> St
  -> [FilePath]
  -> IO St
showImagesSimple f g st images = do
   let (nrows', ncols_) = viewToNRowsCols st
   let ncols' = g ncols_
   let c = st^.currentSlice
   let subset = take (ncols' * nrows') $ drop (c * ncols' * nrows') images
   mapM_ (f (st^.ub)) (enumerate subset)
   pure $ st & displayedImages .~ (takeFileName <$> subset)

showImagesSingle :: St -> [FilePath] -> IO St
showImagesSingle st images = do
  let c = st^.currentSlice
  case images of
    [] -> pure st
    [main_img] -> do
      showImageSingle (st^.ub) (1, main_img)
      pure $ st & displayedImages .~ [takeFileName main_img]
    _ -> do
      case images ^? ix c of
        Nothing -> pure st
        Just main_img -> do
          let other_imgs = drop (c + 1) images
          let subset = take 4 other_imgs
          case subset of
            (x' : xs) -> do
              let i = [x'] <> [main_img] <> xs
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
      xposF = if (px, py) == (1, 0) then 34 else 2 + px * 38
      yposF = 2 + py * 11
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
  when (idx <= 4) $
    showImageInner px py xposF yposF wF hF ub' filepath
    where
      tmp = indexToCoords 3 idx
      (px, py) =
        case tmp of
          (0, 0) -> (1, 0)  -- swap 1
          (1, 0) -> (0, 0)  -- swap 2
          (1, 1) -> (2, 1)  -- shift 1
          _ -> tmp
      xposF = if (px, py) == (1, 0) then 34 else 2 + px * 38
      yposF = 2 + py * 11
      wF px' py' = if (px', py') == (1, 0) then Just 30 else Just 13
      hF px' py' = if (px', py') == (1, 0) then Just 19 else Just 13
