module Download.Downloaders where

import Types
    ( St )
import System.FilePath (takeFileName, takeDirectory)
import Requests ( download )
import Serialization.In (IPCResponses)
import Serialization.Out (Url)

downloadReplicateDir
  :: (Int -> IPCResponses -> IO ())
  -> St
  -> FilePath
  -> [FilePath]
  -> [Url]
  -> IO (Either String St)
downloadReplicateDir cb st dir sorted urls =
  download cb st urls dirs names
    where
      dirs = replicate (length urls) dir
      names = takeFileName <$> sorted

downloadTakeDir
  :: (Int -> IPCResponses -> IO ())
  -> St
  -> [FilePath]
  -> [Url]
  -> IO (Either String St)
downloadTakeDir cb st sorted urls =
  download cb st urls dirs names
    where
      dirs = takeDirectory <$> sorted
      names = takeFileName <$> sorted
