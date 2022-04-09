{-# LANGUAGE LambdaCase #-}

module Download.Common where

import Data.Maybe (fromJust)
import Serialization.In
    ( IPCResponses(Downloaded) )
import Data.List ( sortBy )
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

decodeDownloaded :: IPCResponses -> Either String FilePath
decodeDownloaded = \case
  Downloaded json -> Right json
  _ -> Left "Wrong response type"

fromRight :: Either String p -> p
fromRight (Right x) = x
fromRight (Left e) = error e
