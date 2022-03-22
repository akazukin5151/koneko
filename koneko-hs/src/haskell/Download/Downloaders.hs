module Download.Downloaders where

import Common ( viewToNRowsCols, modeToNRowsCols)
import Core ( enumerate)
import Types
    ( conn,
      St )
import Lens.Micro ((^.))
import System.FilePath (takeFileName, takeDirectory)
import Data.Tuple (swap)
import Download.Common ( download, showImagesConcurrently )

downloadUserIllust :: St -> String -> [FilePath] -> [String] -> IO [FilePath]
downloadUserIllust st dir sorted urls = do
  download (showImagesConcurrently st assoc) (st^.conn) urls dirs names
    where
      (nrows, ncols_) = viewToNRowsCols st
      subset = take (ncols_ * nrows) sorted
      assoc = swap <$> enumerate subset
      dirs = replicate (length urls) dir
      names = takeFileName <$> sorted

downloadIllustDetail :: St -> String -> [FilePath] -> [String] -> IO [FilePath]
downloadIllustDetail st dir sorted urls =
  download (showImagesConcurrently st assoc) (st^.conn) urls dirs names
    where
      -- always 1 main image + 4 side images
      subset = take 5 sorted
      assoc = swap <$> enumerate subset
      dirs = replicate (length urls) dir
      names = takeFileName <$> sorted

downloadUserFollowing :: St -> [FilePath] -> [String] -> IO [FilePath]
downloadUserFollowing st sorted urls =
  download (showImagesConcurrently st assoc') (st^.conn) urls dirs names
    where
      (n_artists, ncols) = modeToNRowsCols st
      n_total_pics = ncols - 1
      -- TODO: rather inefficient to construct the path in parseUserDetailResponse
      -- then deconstruct it again here
      dirs = takeDirectory <$> sorted
      names = takeFileName <$> sorted
      assoc' = swap <$> enumerate (take (n_artists*n_total_pics) sorted)
