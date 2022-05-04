{-# LANGUAGE TemplateHaskell #-}

module Config.Types where

import Lens.Micro.TH (makeLenses)
import Data.Aeson.TH ( deriveJSON )
import Data.Aeson (defaultOptions, Options (fieldLabelModifier))

data GalleryViewConfig =
  GalleryViewConfig
    { galleryViewConfig_image_xcoords :: [Int]
    -- ^ The x-coordinates of every image in a given row
    , galleryViewConfig_image_ycoords :: [Int]
    -- ^ The y-coordinates of every image in a given row
    }
    deriving (Show)

$(deriveJSON defaultOptions
  {fieldLabelModifier = drop (length "galleryViewConfig_")} ''GalleryViewConfig)

data ArtistListViewConfig =
  ArtistListViewConfig
    { artistListViewConfig_image_xcoords :: [Int]
    -- ^ The x-coordinates of every image in a given row
    , artistListViewConfig_image_ycoords :: [Int]
    -- ^ The y-coordinates of every image in a given row
    }
    deriving (Show)

$(deriveJSON defaultOptions
  {fieldLabelModifier = drop (length "artistListViewConfig_")} ''ArtistListViewConfig)

data Config =
  Config
    { _pythonProcessPath :: FilePath
    -- ^ Path to the python process. Probably temporary until I can figure out
    -- distribution and installation
    , _refreshToken :: String
    -- ^ Refresh token for your pixiv account
    , _imageWidth :: Int
    -- ^ Width of all images (except on the home and info pages)
    , _imageHeight :: Int
    -- ^ Height of all images (except on the home and info pages)
    , _boxWidth :: Int
    -- ^ Width of all boxes (around the images)
    , _boxHeight :: Int
    -- ^ Height of all boxes (around the images)
    , _nrows :: Int
    -- ^ Number of rows in a slice for the grid and artist list (the post view is
    -- always fixed at displaying a single image)
    , _ncols :: Int
    -- ^ Number of columns in a slice for the grid (the artist list is always fixed
    -- at one profile pic + 3 previews per row; and the post view is also fixed at
    -- displaying a single image, but this might change)
    , _galleryView :: GalleryViewConfig
    , _artistListView :: ArtistListViewConfig
    }
    deriving (Show)

makeLenses ''Config

$(deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''Config)
