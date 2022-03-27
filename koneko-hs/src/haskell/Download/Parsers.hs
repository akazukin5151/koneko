{-# LANGUAGE OverloadedStrings #-}

module Download.Parsers where

import Core ( enumerate, intToStr)
import System.FilePath ((</>), (<.>))
import Serialization.In
    ( ImageUrl(square_medium, large),
      MetaPage(image_urls),
      UserIllust(title, userIllust_image_url),
      UserIllustResponse(illusts),
      IllustDetailResponse (illustDetailResponse_illust),
      ProfileImageUrl(profileImageUrl_medium),
      User(user_profile_image_urls, user_name),
      UserPreview(userPreview_user, userPreview_illusts),
      UserDetailResponse(userDetailResponse_user_previews, userDetailResponse_next_url),
      IllustDetail(illustDetail_meta_pages, illustDetail_image_urls, illustDetail_title, illustDetail_user), IllustUser (illustUser_name), next_url )
import Data.List ( sort )
import Data.Text ( pack, unpack, splitOn )

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

parseUserIllustResponse
  :: String -> UserIllustResponse -> ([a], [FilePath], [String], Maybe String)
parseUserIllustResponse dir r = ([], sorted, urls, Just n)
    where
      n = next_url r
      illusts' = illusts r
      titles = title <$> illusts'
      -- default sort is by date; enumerate preserves this order
      names = [leftPad idx <> "_" <> title' <.> "jpg" | (idx, title') <- enumerate titles]
      paths = [dir </> name' | name' <- names]
      sorted = sort paths
      urls = square_medium . userIllust_image_url <$> illusts'

parseIllustDetailResponse
  :: String -> IllustDetailResponse -> ([a], [FilePath], [String], Maybe String)
parseIllustDetailResponse dir r = ([], sorted, urls, Nothing)
    where
      meta_pages = illustDetail_meta_pages $ illustDetailResponse_illust r
      -- it's not possible to get the titles without another query, so whatever
      urls = large . image_urls <$> meta_pages
      -- TODO: efficiency
      names = [
          leftPad idx <> "_" <> unpack (last $ splitOn "/" $ pack url)
            | (idx, url) <- enumerate urls]
      paths = [dir </> name' | name' <- names]
      sorted = sort paths

parseUserDetailResponse
  :: String -> UserDetailResponse -> ([String], [FilePath], [String], Maybe String)
parseUserDetailResponse dir r = (user_name <$> users', full_paths, urls, Just n)
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
          )
        | u <- users']

      illust_stuff =
        [ ( square_medium $ illustDetail_image_urls i
          , filter (/= '/') $ illustDetail_title i <.> "jpg"
          , \idx -> dir </> leftPad idx <> "_" <> illustUser_name (illustDetail_user i) </> "previews"
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
      urls = (\(a, _, _) -> a) <$> interleaved
      names = (\(i, (_, a, _)) -> leftPad i <> "_" <> a) <$> enumerate interleaved
      full_paths = zipWith (</>) dirs names
