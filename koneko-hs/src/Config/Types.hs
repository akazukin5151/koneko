{-# LANGUAGE TemplateHaskell #-}

module Config.Types where

import Lens.Micro.TH (makeLenses)
import Data.Aeson.TH ( deriveJSON )
import Data.Aeson (defaultOptions, Options (fieldLabelModifier))

data Config =
  Config
    { _pythonProcessPath :: FilePath
    , _refreshToken :: String
    }
    deriving (Show)

makeLenses ''Config

$(deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''Config)
