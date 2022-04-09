module Download.Downloaders where

import Types
    ( St )
import System.FilePath (takeFileName, takeDirectory)
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
      -- TODO: rather inefficient to construct the path in parseUserDetailResponse
      -- then deconstruct it again here
      dirs = takeDirectory <$> sorted
      names = takeFileName <$> sorted
