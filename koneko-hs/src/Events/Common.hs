module Events.Common where

import Common ( getEditorText )
import Core ( highlightedMode, intToStr )
import Types
    ( history,
      historyIdx,
      ub,
      Mode(ArtistIllustrations, SingleIllustration,
           FollowingArtists, FollowingArtistsIllustrations, SearchArtists,
           RecommendedIllustrations),
      St, konekoDir, currentPage1, your_id )
import Graphics.Ueberzug ( clear )
import Lens.Micro ((^.), (.~), (&), (%~))
import System.FilePath ((</>))
import System.Directory (listDirectory)
import Data.List (sort)
import Data.Maybe ( fromJust )
import Data.Text (unpack)

wrapped :: Int -> (Int -> Int) -> Int -> Int
wrapped lim f x' | 0 <= f x' && f x' <= lim = f x'
                 | otherwise                = x'

clearThenUpdate :: St -> String -> IO St
clearThenUpdate st iden = do
  Right new_ub <- clear (st^.ub) iden
  -- This is a performance hack -- it seemed this made clearing the grid
  -- more visually inconsistent. While the total time to clear all images
  -- were faster on average, individual images are less consistent.
  -- Sometimes all images get cleared at the same time, but sometimes some
  -- images stick around then get cleared. The overall performance increase
  -- was not visually apparent, in fact it looked like it was slower
  -- Not filtering the displayedImages list on every call makes it slightly more consistent
  -- and faster. This works as long as the caller knows exactly what images will be cleared
  -- and manually set displayedImages afterwards. For example if the caller knows
  -- all images will be cleared, simply set it to empty list []
  pure $ st & ub .~ new_ub -- & displayedImages %~ filter (/= iden)

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

listDirectoryFullSorted :: FilePath -> IO [FilePath]
listDirectoryFullSorted dir = do
  filenames <- listDirectory dir
  pure $ (dir </>) <$> sort filenames
