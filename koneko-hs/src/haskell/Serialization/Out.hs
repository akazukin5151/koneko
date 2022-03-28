{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Serialization.Out where

import Data.Aeson
    ( camelTo2, defaultOptions, Options(constructorTagModifier) )
import Data.Aeson.TH ( deriveJSON )

type Url = String

data DownloadInfo =
  DownloadInfo
    { url :: Url
    , path :: FilePath
    , name :: FilePath
    }
    deriving Show

$(deriveJSON defaultOptions ''DownloadInfo)

type RefreshToken = String

data IPCActions = Request PixivRequest
                | Download [DownloadInfo]
                | Login RefreshToken
                | Shutdown
                | ReportLen Int
                deriving (Show)

type UserId = String
type Query = String
type Offset = Int
type ImageId = String

data PixivRequest = UserIllusts UserId Offset
                  | IllustDetail ImageId
                  | UserFollowing UserId Offset
                  | SearchUser Query Offset
                  | IllustFollow Offset
                  | IllustRecommended Offset
                  deriving (Show)

$(deriveJSON defaultOptions {constructorTagModifier = camelTo2 '_'} ''PixivRequest)

$(deriveJSON defaultOptions ''IPCActions)

data IPCJson =
  IPCJson
    { ident :: Int
    , action :: IPCActions
    }
  deriving (Show)

$(deriveJSON defaultOptions ''IPCJson)
