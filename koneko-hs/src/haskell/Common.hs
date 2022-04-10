{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Common where

import Data.List ( intercalate )
import Lens.Micro ( (&), (.~), (^.), (<&>) )
import Types
    ( activeView,
      editor,
      footer,
      Field,
      Mode(SingleIllustration, FollowingArtists, SearchArtists,
           FollowingArtistsIllustrations, RecommendedIllustrations, ArtistIllustrations, Home),
      St,
      View(WelcomeView, GalleryView, ArtistListView, PostView), config )
import Data.Char (isDigit)
import Brick.Widgets.Edit (getEditContents)
import Brick ( Widget, txt )
import qualified Data.Text as T
import Data.Text (Text, splitOn, unpack)
import Control.Arrow ((>>>))
import Text.Read (readMaybe)
import Maybes (firstJusts)
import Control.Monad ((>=>))
import Data.Bifunctor (Bifunctor (first))
import Core (highlightedMode)
import Config.Types (ncols, nrows)

-- | Bind a function to the second argument of a bifunctor, then append
-- a value to the first argument of the bifunctor
--
-- A monadic bind @a >>= b@ will map @b@ onto @a@ then @join@ the result,
-- but do nothing if @a@ is an "empty" or "falsey" value (like Left).
--
-- This is sometimes a problem for Either, because the Left value is sometimes
-- too vague. This function lets you annotate where this Left value happened, and
-- provide additional context, by @mappend@ing a value to
-- the Left value (more generally, the second argument of the bifunctor) after
-- doing the bind
--
-- Examples:
-- >>> let f x = if even x then Right x else Left "odd"
-- >>> bindWithMsg f "Initial value was odd: " $ Right 3
-- Left "Initial value was odd: odd"
bindWithMsg :: (Bifunctor p, Semigroup a, Monad (p a))
            => (b -> p a c) -- ^ The function to map to the
                            -- second argument of the bifunctor
            -> a            -- ^ The message to @mappend@ to the
                            -- first argument of the bifunctor
            -> p a b        -- ^ The initial bifunctor
            -> p a c
bindWithMsg f msg x = first (msg <>) (x >>= f)

-- more specific utils
-- coordsToIndex is the partial inverse function of indexToCoords
coordsToIndex :: Int -> Int -> Int -> Int -> Maybe Int
coordsToIndex ncols' nrows' x' y' =
  [ (x' `rem` ncols') + (ncols' * y')
    | 0 <= x' && x' <= ncols' && 0 <= y' && y' <= nrows' ]

-- >>> indexToCoords 5 0
-- (0, 0)
-- >>> indexToCoords 5 4
-- (4, 0)
-- >>> indexToCoords 5 5
-- (0, 1)
-- >>> indexToCoords 5 9
-- (4, 1)
-- indexToCoords is the inverse function of coordsToIndex
indexToCoords :: Int -> Int -> (Int, Int)
indexToCoords ncols' idx = (x', y')
  where
    x' = idx `mod` ncols'
    y' = idx `div` ncols'

validInput :: Mode -> String -> Bool
validInput ArtistIllustrations = all isDigit
validInput SingleIllustration = all isDigit
validInput _ = const True

viewToNRowsCols :: St -> (Int, Int)
viewToNRowsCols st =
  case st^.activeView of
    ArtistListView -> (st^.config.nrows, 5)
    PostView -> (2, 3)
    _ -> (st^.config.nrows, st^.config.ncols)

-- | Uses highlighted mode, which is not necessarily the current mode
modeToNRowsCols :: St -> (Int, Int)
modeToNRowsCols st =
  case modeToView $ highlightedMode st of
    ArtistListView -> (st^.config.nrows, 5)
    PostView -> (2, 3)
    _ -> (st^.config.nrows, st^.config.ncols)

modeToView :: Mode -> View
modeToView ArtistIllustrations           = GalleryView
modeToView SingleIllustration            = PostView
modeToView FollowingArtists              = ArtistListView
modeToView FollowingArtistsIllustrations = GalleryView
modeToView SearchArtists                 = ArtistListView
modeToView RecommendedIllustrations      = GalleryView
modeToView Home                          = WelcomeView

getEditorText :: St -> Text
getEditorText st = T.unwords $ getEditContents $ st^.editor

defaultViewFooter :: St -> Widget Field
defaultViewFooter st =
  case st^.activeView of
    -- illustfollow: [a]rtist illusts
    GalleryView ->
      txt " [o]pen in browser [v]iew [d]ownload  [n]ext [p]previous  [b]ack [q]uit"
    -- TODO: illusts when profile pic highlighted, view when previews
    -- highlighted
    ArtistListView ->
      txt " [i]llusts  [n]ext [p]revious  [b]ack [q]uit"
    PostView ->
      txt " [o]pen in browser [f]ull res [d]ownload [r]elated  [n]ext [p]revious  [b]ack [q]uit"
    WelcomeView -> initialFooter

initialFooter :: Widget n
initialFooter =
  txt "[jk] navigate | focus [h]istory | [enter] select | [q]uit"

updateFooter :: St -> St
updateFooter st = st & footer .~ defaultViewFooter st

-- | Get the next offset from the next url
-- >>> nextOffset "a=b&offset=2&c=d"
-- Just 2
-- >>> nextOffset "a=b&offset=c&offset=1"
-- Just 1
nextOffset :: Text -> Maybe Int
nextOffset next_url =
  splitOn "&" next_url
  <&> splitOn "="
  & filter keyIsOffset
  <&> (\case {[_, v] -> Just v; _ -> Nothing}
       >=> (unpack >>> readMaybe))
  & firstJusts

keyIsOffset :: [Text] -> Bool
keyIsOffset ["offset", _] = True
keyIsOffset _             = False

logger :: Show a => [Char] -> a -> IO ()
logger label x = appendFile "log.txt" $ label <> " " <> show x <> "\n"

loggerLst label xs = appendFile "log.txt" $ label <> " = [" <> intercalate "," xs <> "]\n"

loggerStr label xs = appendFile "log.txt" $ label <> xs <> "\n"
