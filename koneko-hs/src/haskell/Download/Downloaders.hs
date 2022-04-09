module Download.Downloaders where

import Common ( viewToNRowsCols, modeToNRowsCols)
import Core ( enumerate)
import Types
    ( St )
import System.FilePath (takeFileName, takeDirectory)
import Data.Tuple (swap)
import Requests ( download )
import Serialization.In (IPCResponses)
import Serialization.Out (Url)

emptyCb :: Applicative f => (a1, b) -> p -> f (a1, [a2])
emptyCb (a, _) _ = pure (a, [])

-- TODO: explore eta reduce potential
-- emptyCb
downloadUserIllust'
  :: (Int -> IPCResponses -> IO ())
  -> St
  -> [Char]
  -> [FilePath]
  -> [Url]
  -> IO (Either String St)
downloadUserIllust' cb st dir sorted urls = download cb st urls dirs names
    where
      dirs = replicate (length urls) dir
      names = takeFileName <$> sorted

-- (showImagesConcurrently st assoc)
downloadUserIllust :: (Int -> IPCResponses -> IO ())
  -> St -> [Char] -> [FilePath] -> [Url] -> IO (Either String St)
downloadUserIllust cb st dir sorted urls = do
  download cb st urls dirs names
    where
      (nrows, ncols_) = viewToNRowsCols st
      subset = take (ncols_ * nrows) sorted
      assoc = swap <$> enumerate subset
      dirs = replicate (length urls) dir
      names = takeFileName <$> sorted

-- emptyCb
downloadIllustDetail' :: (Int -> IPCResponses -> IO ())
  -> St -> [Char] -> [FilePath] -> [Url] -> IO (Either String St)
downloadIllustDetail' cb st dir sorted urls =
  download cb st urls dirs names
    where
      dirs = replicate (length urls) dir
      names = takeFileName <$> sorted

-- (showImagesConcurrently st assoc)
downloadIllustDetail :: (Int -> IPCResponses -> IO ())
  -> St -> [Char] -> [FilePath] -> [Url] -> IO (Either String St)
downloadIllustDetail cb st dir sorted urls =
  download cb st urls dirs names
    where
      -- always 1 main image + 4 side images
      subset = take 5 sorted
      assoc = swap <$> enumerate subset
      dirs = replicate (length urls) dir
      names = takeFileName <$> sorted

-- emptyCb
downloadUserFollowing' :: (Int -> IPCResponses -> IO ())
  -> St -> [FilePath] -> [Url] -> IO (Either String St)
downloadUserFollowing' cb st sorted urls =
  download cb st urls dirs names
    where
      dirs = takeDirectory <$> sorted
      names = takeFileName <$> sorted

-- (showImagesConcurrently st assoc')
downloadUserFollowing :: (Int -> IPCResponses -> IO ())
  -> St -> [FilePath] -> [Url] -> IO (Either String St)
downloadUserFollowing cb st sorted urls =
  download cb st urls dirs names
    where
      (n_artists, ncols) = modeToNRowsCols st
      n_total_pics = ncols - 1
      -- TODO: rather inefficient to construct the path in parseUserDetailResponse
      -- then deconstruct it again here
      dirs = takeDirectory <$> sorted
      names = takeFileName <$> sorted
      assoc' = swap <$> enumerate (take (n_artists*n_total_pics) sorted)
