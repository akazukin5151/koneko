module Events.Common where

import Common ( getEditorText )
import Core ( highlightedMode, intToStr )
import Types
    ( history,
      historyIdx,
      Mode(ArtistIllustrations, PixivPost,
           FollowingArtists, FollowingArtistsIllustrations, SearchArtists,
           RecommendedIllustrations),
      St, konekoDir, currentPage1, your_id )
import Lens.Micro ((^.), (&), (%~), (<&>))
import System.FilePath ((</>))
import System.Directory (listDirectory)
import Data.List (sort)
import Data.Maybe ( fromJust )
import Data.Text (unpack)

wrapped :: Int -> (Int -> Int) -> Int -> Int
wrapped lim f x' | 0 <= f x' && f x' <= lim = f x'
                 | otherwise                = x'

historyDown :: St -> St
historyDown st = st & historyIdx %~ wrapped (length historyHighlighted - 1) (+1)
  where
    historyHighlighted =
      filter (\(mode, _) -> mode == highlightedMode st) $ st^.history

historyUp :: St -> St
historyUp st = st & historyIdx %~ wrapped (length historyHighlighted - 1) (subtract 1)
  where
    historyHighlighted =
      filter (\(mode, _) -> mode == highlightedMode st) $ st^.history

getDirectoryFromMode :: St -> Mode -> Maybe FilePath
getDirectoryFromMode st mode =
  case mode of
    ArtistIllustrations ->
      Just $ st^.konekoDir </> unpack (getEditorText st)
    FollowingArtistsIllustrations ->
      Just $ st^.konekoDir </> "illustfollow"
    RecommendedIllustrations ->
      Just $ st^.konekoDir </> "recommended"
    SearchArtists ->
      Just $ st^.konekoDir </> "search" </> unpack (getEditorText st)
    FollowingArtists ->
      (st^.your_id) <&> \x -> st^.konekoDir </> "following" </> x <> "_new"
    PixivPost ->
      Just $ st^.konekoDir </> "individual" </> unpack (getEditorText st)

getDirectory :: St -> Maybe FilePath
getDirectory st =
  getDirectoryFromMode st $ highlightedMode st

getFirstDirectory :: St -> Mode -> Maybe FilePath
getFirstDirectory st mode =
  case mode of
    PixivPost -> getDirectoryFromMode st mode
    _ -> getDirectoryFromMode st mode <&> (</> intToStr (st^.currentPage1))

getNextDirectory :: St -> Maybe FilePath
getNextDirectory st =
  case highlightedMode st of
    PixivPost -> getDirectory st
    _ -> getDirectory st <&> (</> intToStr (st^.currentPage1 + 1))

listDirectoryFullSorted :: FilePath -> IO [FilePath]
listDirectoryFullSorted dir = do
  filenames <- listDirectory dir
  pure $ (dir </>) <$> sort filenames
