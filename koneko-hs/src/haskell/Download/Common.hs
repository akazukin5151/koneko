{-# LANGUAGE LambdaCase #-}

module Download.Common where

import Common ( bindWithMsg )
import Sockets ( catchSomeException, recvAll, sendAll')
import Data.Maybe (fromJust)
import Serialization.In
    ( IPCResponse(response), IPCResponses(Downloaded) )
import Serialization.Out
    ( DownloadInfo(DownloadInfo, url, path, name),
      IPCActions(Download),
      IPCJson(IPCJson, action) )
import Data.List ( sortBy )
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)
import Network.Socket ( Socket )
import Control.Monad (foldM)
import Data.Aeson ( eitherDecodeStrict, encode )
import Control.Arrow ( (>>>) )
import Data.ByteString.Lazy (toStrict)
import Data.Function ((&))
import Data.Bifunctor (Bifunctor(first))
import Data.Functor ((<&>))
import Data.Either (rights)
import System.Directory (createDirectoryIfMissing)
import Events.ShowImages ( showImageView )
import Types ( ub, St )
import Lens.Micro ((^.))

showImagesConcurrently
  :: St -> [(String, Int)] -> (Int, [String]) -> String -> IO (Int, [String])
showImagesConcurrently st = showImages'' (actuallyDisplay st)

actuallyDisplay :: St -> Int -> FilePath -> IO ()
actuallyDisplay st idx path = do
  _ <- showImageView st (st^.ub) (idx, path)
  pure ()

-- |
-- >>> sequence_ [foldM_ (showImages'' (\p i -> print i) [("0", 0), ("1", 1), ("2", 2)]) (0, []) perm | perm <- permutations ["0", "1", "2"]]
-- 0
-- 1
-- 2
-- -- Repeat for 6 times
showImages''
  :: (Monad m, Num a, Ord a, Eq b)
  => (a -> b -> m c)
  -> [(b, a)]
  -> (a, [b])
  -> b
  -> m (a, [b])
showImages'' g assoc (currentIdx, prev_not_shown) path = do
  -- lookup can fail because we're only displaying the current slice, not the entire
  -- page
  case lookup path assoc of
    Nothing                     -> pure (currentIdx, prev_not_shown)
    Just idx | idx > currentIdx -> pure (currentIdx, path : prev_not_shown)
    Just idx | idx < currentIdx -> pure (currentIdx, prev_not_shown)
    Just _ -> do
      g currentIdx path

      let prev_with_idx =
            sortByFst $ map (\x -> (fromJust $ lookup x assoc, x)) prev_not_shown

      d <- displayPrevInOrder g (currentIdx + 1) prev_with_idx

      -- prev_not_shown isn't updated here, but it doesn't really matter because
      -- it won't show if idx is less than currentIdx
      pure (d, prev_not_shown)

displayPrevInOrder :: (Eq a, Num a, Monad m) => (a -> b -> m c) -> a -> [(a, b)] -> m a
displayPrevInOrder _ i []                  = pure i
displayPrevInOrder g cur_idx ((i, p) : xs) = do
  if cur_idx == i
     then g cur_idx p *> displayPrevInOrder g (cur_idx + 1) xs
     else pure cur_idx

sortByFst :: Ord a => [(a, b)] -> [(a, b)]
sortByFst = sortBy (\(a, _) (b, _) -> compare a b)

download
  :: Num a
  => ((a, [b]) -> FilePath -> IO (a, [b])) -- ^ callback drawing function
  -> Socket
  -> [String] -- ^ urls to download
  -> [FilePath] -- ^ output paths
  -> [String] -- ^ output names
  -> IO [FilePath]
download cb conn urls dirs names = do
  mapM_ (createDirectoryIfMissing True) dirs
  tryAll cb conn total $ toStrict $ encode (IPCJson {action = Download info})
  where
    total = length names
    info =
        [ DownloadInfo
           { url = url'
           , path = dir
           , name = name'
           }
        | (url', dir, name') <- zip3 urls dirs names]

tryAll
  :: Num a
  => ((a, [b]) -> FilePath -> IO (a, [b]))
  -> Socket
  -> Int
  -> ByteString
  -> IO [FilePath]
tryAll cb conn total text = do
  _i <- catchSomeException (sendAll' conn text)
  recvToTotal cb conn total [] (0, [])

recvToTotal :: (a -> FilePath -> IO a) -> Socket -> Int -> [FilePath] -> a -> IO [FilePath]
recvToTotal cb conn total res arg = do
  d <- recvAll conn
  -- TODO: arguably, the download functions should return Either
  -- flatten the either from recvAll and decoding
  let files =
        d
        -- This crashes if recvAll failed
        & fromRight
        & B.lines
        <&> (\bytestring ->
              bytestring
                & eitherDecodeStrict
                & first ("Decoding bytestring into IPCResponse failed: " <>)
                & bindWithMsg (response >>> decodeDownloaded)
                    "Decoding Download from IPCResponse failed: "
          )
        -- this ignores all Lefts (decode failures ignored)
        & rights

  let new = files <> res
  newImageIdx <- foldM cb arg new
  if length new < total
     then recvToTotal cb conn total new newImageIdx
     else pure new

decodeDownloaded :: IPCResponses -> Either String FilePath
decodeDownloaded = \case
  Downloaded json -> Right json
  _ -> Left "Wrong response type"

fromRight :: Either String p -> p
fromRight (Right x) = x
fromRight (Left e) = error e
