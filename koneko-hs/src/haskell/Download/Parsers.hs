{-# LANGUAGE OverloadedStrings #-}

module Download.Parsers where

import Core ( enumerate, intToStr)
import System.FilePath ((</>), (<.>))
import Serialization.In
    ( IllustDetail(illustDetail_meta_single_page,
                   illustDetail_meta_pages, illustDetail_image_urls,
                   illustDetail_title, illustDetail_user, illustDetail_id),
      IllustDetailResponse(illustDetailResponse_illust),
      IllustUser(illustUser_name),
      ImageUrl(large, original, square_medium),
      MetaPage(image_urls),
      MetaSinglePage(original_image_url),
      ProfileImageUrl(profileImageUrl_medium),
      User(user_profile_image_urls, user_name),
      UserDetailResponse(userDetailResponse_next_url,
                         userDetailResponse_user_previews),
      UserIllustResponse(next_url, illusts),
      UserPreview(userPreview_user, userPreview_illusts) )
import Data.List ( sort )
import Data.Text ( pack, unpack, splitOn )
import Types (Request(..))
import Control.Applicative ((<|>))

leftPad :: Int -> String
leftPad num | num <= 9 = "0" <> intToStr num
            | otherwise = intToStr num

-- >>> interleave [0..30] [30..(30*3 + 30 - 1)]
-- [0,30,31,32,1,33,34,35,2,36,37,38,3,39,40,41,4,42,43,44,5,45,46,47,6,48,49,50,7,51,52,53,8,54,55,56,9,57,58,59,10,60,61,62,11,63,64,65,12,66,67,68,13,69,70,71,14,72,73,74,15,75,76,77,16,78,79,80,17,81,82,83,18,84,85,86,19,87,88,89,20,90,91,92,21,93,94,95,22,96,97,98,23,99,100,101,24,102,103,104,25,105,106,107,26,108,109,110,27,111,112,113,28,114,115,116,29,117,118,119]
interleave :: [a] -> [a] -> [a]
interleave xs ys = concat [[x] <> sub | (x, sub) <- zip xs $ window3 ys]

-- >>> window3 [0..9]
-- [[0,1,2],[3,4,5],[6,7,8],[9]]
--
-- >>> window3 [0..10]
-- [[0,1,2],[3,4,5],[6,7,8],[9,10]]
--
-- >>> window3 [0..11]
-- [[0,1,2],[3,4,5],[6,7,8],[9,10,11]]
--
-- prop> all (<= 3) $ fmap length (window3 xs)
window3 :: [a] -> [[a]]
window3 [] = []
window3 [x] = [[x]]
window3 [a, b] = [[a, b]]
window3 (a : b : c : xs) = [[a, b, c]] <> window3 xs

parseUserIllustResponse :: String -> UserIllustResponse -> Request
parseUserIllustResponse dir r =
  Request
    { labels_ = []
    , paths = sorted
    , urls = urls'
    , nextUrl_ = n
    , image_ids = Just <$> image_ids'
    , original_urls = original_urls'
    }
    where
      original_urls' = zipWith (<|>) originals_single originals_meta
      originals_single = original_image_url . illustDetail_meta_single_page <$> illusts'
      -- multiple meta pages = multiple images in a post
      -- but for downloading from this mode, only download the first image in the post
      originals_meta = original . image_urls . head . illustDetail_meta_pages <$> illusts'
      image_ids' = illustDetail_id <$> illusts'
      n = next_url r
      illusts' = illusts r
      titles = illustDetail_title <$> illusts'
      -- default sort is by date; enumerate preserves this order
      names = [leftPad idx <> "_" <> title' <.> "jpg" | (idx, title') <- enumerate titles]
      paths' = [dir </> name' | name' <- names]
      sorted = sort paths'
      urls' = square_medium . illustDetail_image_urls <$> illusts'

parseIllustDetailResponse :: String -> IllustDetailResponse -> Request
parseIllustDetailResponse dir r =
  Request
    { labels_ = []
    , paths = sorted
    , urls = urls'
    , nextUrl_ = Nothing
    , image_ids = [Just image_ids']
    , original_urls = original_urls'
    }
    where
      original_urls' =
        case originals_meta of
          [] -> [originals_single]
          xs -> xs
      originals_single = original_image_url . illustDetail_meta_single_page $ illusts'
      originals_meta = original . image_urls <$> meta_pages'
      illusts' = illustDetailResponse_illust r
      image_ids' = illustDetail_id illusts'
      meta_pages' = illustDetail_meta_pages illusts'
      -- if there are multiple pages, use meta_pages -> image_urls -> large
      -- if there is only one page, use image_urls -> large
      urls'  = case meta_pages' of
                [] -> [large $ illustDetail_image_urls illusts']
                xs -> large . image_urls <$> xs
      -- TODO: efficiency
      names = [
          leftPad idx <> "_" <> unpack (last $ splitOn "/" $ pack url)
            | (idx, url) <- enumerate urls']
      paths' = [dir </> name' | name' <- names]
      sorted = sort paths'

parseUserDetailResponse :: String -> UserDetailResponse -> Request
parseUserDetailResponse dir r =
  Request
    { labels_ = user_name <$> users'
    , paths = full_paths
    , urls = urls'
    , nextUrl_ = n
    , image_ids = image_ids'
    , original_urls = original_urls'
    }
    where
      n = userDetailResponse_next_url r
      -- adapted from show Images Simple
      user_previews = userDetailResponse_user_previews r
      users' = userPreview_user <$> user_previews
      illusts' = userPreview_illusts <$> user_previews

      user_stuff =
        [ ( profileImageUrl_medium $ user_profile_image_urls u
          , user_name u <.> "jpg"
          , \idx -> dir </> leftPad idx <> "_" <> user_name u </> "profile"
          , Nothing
          , Nothing
          )
        | u <- users']

      illust_stuff =
        [
        let originals_single = original_image_url . illustDetail_meta_single_page $ i
            -- multiple meta pages = multiple images in a post
            -- but for downloading from this mode, only download the first image
            -- in the post
            originals_meta = original . image_urls . head . illustDetail_meta_pages $ i in
          ( square_medium $ illustDetail_image_urls i
          , filter (/= '/') $ illustDetail_title i <.> "jpg"
          , \idx ->
              dir
              </> leftPad idx <> "_" <> illustUser_name (illustDetail_user i) </> "previews"
          , Just $ illustDetail_id i
          , originals_single <|> originals_meta
          )
        | i <- concat illusts'
        ]

      -- length user_base_dirs == length users' == 30
      user_base_dirs =
        [dir </> leftPad idx <> "_" <> user_name u | (idx, u) <- enumerate users']

      ll = length $ head illusts'

      dirs = concat
          [ [user_base_dir </> "profile"]
            <> replicate ll (user_base_dir </> "previews")
          | user_base_dir <- user_base_dirs
          ]

      interleaved = interleave user_stuff illust_stuff
      urls' = (\(a, _, _, _, _) -> a) <$> interleaved
      image_ids' = (\(_, _, _, a, _) -> a) <$> interleaved
      original_urls' = (\(_, _, _, _, a) -> a) <$> interleaved
      names = (\(i, (_, a, _, _, _)) -> leftPad i <> "_" <> a) <$> enumerate interleaved
      full_paths = zipWith (</>) dirs names
