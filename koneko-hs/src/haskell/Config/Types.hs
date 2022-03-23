{-# LANGUAGE TemplateHaskell #-}

module Config.Types where

import Lens.Micro.TH (makeLenses)
import Data.Aeson.TH ( deriveJSON )
import Data.Aeson (defaultOptions, Options (fieldLabelModifier))

data Config =
  Config
    { _pythonProcessPath :: FilePath
    -- ^ Path to the python process. Probably temporary until I can figure out
    -- distribution and installation
    , _refreshToken :: String
    -- ^ Refresh token for your pixiv account
    , _imageWidth :: Int
    -- ^ Width of all images (except on the welcome and info pages)
    , _imageHeight :: Int
    -- ^ Height of all images (except on the welcome and info pages)
    , _nrows :: Int
    -- ^ Number of rows in a slice for the grid and artist list (the post view is
    -- always fixed at displaying a single image)
    , _ncols :: Int
    -- ^ Number of columns in a slice for the grid (the artist list is always fixed
    -- at one profile pic + 3 previews per row; and the post view is also fixed at
    -- displaying a single image, but this might change)
    }
    deriving (Show)

makeLenses ''Config

$(deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''Config)
