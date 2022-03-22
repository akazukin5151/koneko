module Config.Core where

import Data.Aeson

parseConfig path = do
  eitherDecodeFileStrict path
