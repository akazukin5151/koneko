{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Serialization.In where

import Data.Aeson ( defaultOptions, Options(fieldLabelModifier) )
import Data.Aeson.TH ( deriveJSON )

data PythonError = NotLoggedIn
                 | OtherError
                 deriving (Show)

$(deriveJSON defaultOptions ''PythonError)

data MetaPage = MetaPage { image_urls :: ImageUrl }
    deriving Show

data ImageUrl =
  ImageUrl
    { square_medium :: String
    , medium :: String
    , large :: String
    , original :: Maybe String
    }
    deriving Show

$(deriveJSON defaultOptions ''ImageUrl)

$(deriveJSON defaultOptions ''MetaPage)

data LoginUser = LoginUser { _LoginUser_id :: String }
  deriving Show

$(deriveJSON defaultOptions
  {fieldLabelModifier = drop (length "_LoginUser_")} ''LoginUser)

data LoginResponse = LoginResponse { _LoginResponse_user :: LoginUser }
  deriving Show

$(deriveJSON defaultOptions
  {fieldLabelModifier = drop (length "_LoginResponse_")} ''LoginResponse)

data IPCResponses = Requested String
                  | Downloaded FilePath
                  | LoginInfo LoginResponse
                  | Exit
                  | Error PythonError
                  | ReportLen Int
                  deriving (Show)

$(deriveJSON defaultOptions ''IPCResponses)

data IPCResponse =
  IPCResponse
    { ident :: Int
    , response :: IPCResponses
    }
  deriving (Show)

$(deriveJSON defaultOptions ''IPCResponse)

data UserIllustResponse =
  UserIllustResponse
    { illusts :: [UserIllust]
    , next_url :: String
    }
    deriving (Show)

data UserIllust =
  UserIllust
    { userIllust_id :: Int
    , title :: String
    , userIllust_image_url :: ImageUrl
    , caption :: String
    , user :: IllustUser
    , tags :: [Tag]
    , tools :: [String]
    , create_date :: String
    , page_count :: Int
    , total_view :: Int
    , total_bookmarks :: Int
    , is_bookmarked :: Bool
    }
    deriving (Show)

data Tag =
  Tag
    { tag_name :: String
    , translated_name :: Maybe String
    }
    deriving (Show)

data IllustUser =
  IllustUser
    { illustUser_id :: Int
    , illustUser_name :: String
    , illustUser_is_followed :: Bool
    }
    deriving (Show)

$(deriveJSON defaultOptions
    {fieldLabelModifier = \str -> if str == "tag_name" then "name" else str} ''Tag)

$(deriveJSON defaultOptions
    {fieldLabelModifier = drop (length "illustUser_")} ''IllustUser)

$(deriveJSON defaultOptions
    { fieldLabelModifier =
       \case
         "userIllust_id" -> "id"
         "userIllust_image_url" -> "image_urls"
         x -> x
    } ''UserIllust)

$(deriveJSON defaultOptions ''UserIllustResponse)

-- Doesn't need the image_urls key because it's in meta_pages already
data IllustDetail =
  IllustDetail
    { illustDetail_id :: Int
    , illustDetail_title :: String
    , illustDetail_image_urls :: ImageUrl
    , illustDetail_caption :: String
    , illustDetail_user :: IllustUser
    , illustDetail_tags :: [Tag]
    , illustDetail_tools :: [String]
    , illustDetail_create_date :: String
    , illustDetail_page_count :: Int
    , illustDetail_meta_pages :: [MetaPage]
    , illustDetail_total_view :: Int
    , illustDetail_total_bookmarks :: Int
    , illustDetail_is_bookmarked :: Bool
    }
    deriving Show

$(deriveJSON defaultOptions
    {fieldLabelModifier = drop (length "illustDetail_")} ''IllustDetail)

data IllustDetailResponse =
  IllustDetailResponse
    { illustDetailResponse_illust :: IllustDetail }
    deriving Show

$(deriveJSON defaultOptions
    {fieldLabelModifier = drop (length "illustDetailResponse_")} ''IllustDetailResponse)

data UserDetailResponse =
  UserDetailResponse
    { userDetailResponse_user_previews :: [UserPreview]
    , userDetailResponse_next_url :: Maybe String
    }
    deriving Show

data UserPreview =
  UserPreview
    { userPreview_user :: User
    , userPreview_illusts :: [IllustDetail]
    }
    deriving Show

data User =
  User
    { user_id :: Int
    , user_name :: String
    , user_profile_image_urls :: ProfileImageUrl
    -- TODO: does other modes need this too?
    , user_is_followed :: Bool
    }
    deriving Show

data ProfileImageUrl = ProfileImageUrl { profileImageUrl_medium :: String }
  deriving Show

$(deriveJSON defaultOptions
    {fieldLabelModifier = drop (length "profileImageUrl_")} ''ProfileImageUrl)

$(deriveJSON defaultOptions
    {fieldLabelModifier = drop (length "user_")} ''User)

$(deriveJSON defaultOptions
    {fieldLabelModifier = drop (length "userPreview_")} ''UserPreview)

$(deriveJSON defaultOptions
    {fieldLabelModifier = drop (length "userDetailResponse_")} ''UserDetailResponse)
