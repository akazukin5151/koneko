-- This will be where images are downloaded as well
module Events.FindImages where

import Types
    ( St,
      Mode(SingleIllustration, ArtistIllustrations,
           FollowingArtistsIllustrations, RecommendedIllustrations,
           SearchArtists, FollowingArtists),
      konekoDir, currentPage1, your_id )
import Control.Monad (zipWithM)
import Lens.Micro ((^.), (<&>))
import System.FilePath ((</>))
import System.Directory (listDirectory)
import Common (getEditorText)
import Core ( intToStr )
import Events.Common (listDirectoryFullSorted)
import Data.Maybe (fromJust)
import Data.Text (unpack)

listDirectoryFull :: FilePath -> IO [FilePath]
listDirectoryFull dir = do
  filenames <- listDirectory dir
  pure $ (dir </>) <$> filenames

findImagesView :: Mode -> St -> IO ([FilePath], [FilePath])
findImagesView mode =
  case mode of
    ArtistIllustrations -> findImagesArtistIllustrations
    FollowingArtistsIllustrations -> findImagesFollowingIllustrations
    RecommendedIllustrations -> findImagesRecommendedIllustrations
    SearchArtists -> findSearchArtists
    FollowingArtists -> findFollowingArtists
    SingleIllustration -> findSingleImages

findSingleImages :: St -> IO ([a], [FilePath])
findSingleImages st = do
  let image_id = unpack $ getEditorText st
  -- TODO: posts with single images don't have a directory ... unless if i download
  -- single-image-posts into a dir as well
  let dir = st^.konekoDir </> "individual" </> image_id
  res <- listDirectoryFullSorted dir
  pure ([], res)

findImagesArtistIllustrations :: St -> IO ([a], [FilePath])
findImagesArtistIllustrations st = do
  let artist_id = unpack $ getEditorText st
  -- editor would be validated anyway so no need to handle read failure
  let dir = st^.konekoDir </> artist_id </> intToStr (st^.currentPage1)
  res <- listDirectoryFullSorted dir
  pure ([], res)

findImagesFollowingIllustrations :: St -> IO ([a], [FilePath])
findImagesFollowingIllustrations st = do
  let dir = st^.konekoDir </> "illustfollow" </> intToStr (st^.currentPage1)
  res <- listDirectoryFullSorted dir
  pure ([], res)

findImagesRecommendedIllustrations :: St -> IO ([a], [FilePath])
findImagesRecommendedIllustrations st = do
  let dir = st^.konekoDir </> "recommended" </> intToStr (st^.currentPage1)
  res <- listDirectoryFullSorted dir
  pure ([], res)

findSearchArtists :: St -> IO ([FilePath], [FilePath])
findSearchArtists st = do
  -- Hopefully I'll directly get a list of names from IPC rather than from filenames
  let searchstr = unpack $ getEditorText st
  let dir = st^.konekoDir </> "search" </> searchstr </> intToStr (st^.currentPage1)
  findCommon dir st

findFollowingArtists :: St -> IO ([FilePath], [FilePath])
findFollowingArtists st = do
  -- Get from pixiv API
  -- _new removes the .koneko file
  let user_id = fromJust (st^.your_id) <> "_new"
  let dir = st^.konekoDir </> "following" </> user_id </> intToStr (st^.currentPage1)
  findCommon dir st

findCommon :: FilePath -> p -> IO ([FilePath], [FilePath])
findCommon dir st = do
  artist_dirs <- listDirectoryFullSorted dir
  let profile_dirs = artist_dirs <&> (</> "profile")
  profile_fns <- mapM (fmap head . listDirectory) profile_dirs
  let profile_imgs = zipWith (</>) profile_dirs profile_fns
  res <- findImagesSearchArtist artist_dirs profile_imgs st
  pure (profile_fns, res)

findImagesSearchArtist :: [FilePath] -> [FilePath] -> p -> IO [FilePath]
findImagesSearchArtist artist_dirs profile_imgs _ = do
  let preview_dirs = artist_dirs <&> (</> "previews")
  imgs_by_artists <-
        zipWithM (\profile_img preview_dir -> do
          preview_imgs <- listDirectoryFullSorted preview_dir
          pure $ [profile_img] <> preview_imgs) profile_imgs preview_dirs
  pure $ concat imgs_by_artists
