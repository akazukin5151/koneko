module Events.Common where

import Common ( getEditorText )
import Core ( highlightedMode, intToStr )
import Types
    ( history,
      historyIdx,
      Mode(ArtistIllustrations, SingleIllustration,
           FollowingArtists, FollowingArtistsIllustrations, SearchArtists,
           RecommendedIllustrations),
      St, konekoDir, currentPage1, your_id )
import Lens.Micro ((^.), (&), (%~))
import System.FilePath ((</>))
import System.Directory (listDirectory)
import Data.List (sort)
import Data.Maybe ( fromJust )
import Data.Text (unpack)

wrapped :: Int -> (Int -> Int) -> Int -> Int
wrapped lim f x' | 0 <= f x' && f x' <= lim = f x'
                 | otherwise                = x'

historyDown :: St -> St
historyDown st = st & historyIdx %~ wrapped (length (st ^. history) - 1) (+1)

historyUp :: St -> St
historyUp st = st & historyIdx %~ wrapped (length (st ^. history) - 1) (subtract 1)

getDirectory :: St -> FilePath
getDirectory st =
  case highlightedMode st of
    ArtistIllustrations ->
      st^.konekoDir </> unpack (getEditorText st)
    FollowingArtistsIllustrations ->
      st^.konekoDir </> "illustfollow"
    RecommendedIllustrations ->
      st^.konekoDir </> "recommended"
    SearchArtists ->
      st^.konekoDir </> "search" </> unpack (getEditorText st)
    FollowingArtists ->
      st^.konekoDir </> "following" </> fromJust (st^.your_id) <> "_new"
    SingleIllustration ->
      st^.konekoDir </> "individual" </> unpack (getEditorText st)

getFirstDirectory :: St -> FilePath
getFirstDirectory st =
  case highlightedMode st of
    SingleIllustration -> getDirectory st
    _ -> getDirectory st </> intToStr (st^.currentPage1)

getNextDirectory :: St -> FilePath
getNextDirectory st =
  case highlightedMode st of
    SingleIllustration -> getDirectory st
    _ -> getDirectory st </> intToStr (st^.currentPage1 + 1)

listDirectoryFullSorted :: FilePath -> IO [FilePath]
listDirectoryFullSorted dir = do
  filenames <- listDirectory dir
  pure $ (dir </>) <$> sort filenames
