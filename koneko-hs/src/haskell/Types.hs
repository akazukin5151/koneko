{-# LANGUAGE TemplateHaskell #-}

module Types where

import Graphics.Ueberzug (Ueberzug)
import Lens.Micro.TH (makeLenses)
import Brick.Widgets.Edit (Editor)
import Brick.BChan (BChan)
import Network.Socket (Socket)
import Config.Types ( Config )
import Brick (Widget)
import Data.Text (Text)

data View = GalleryView
          | WelcomeView
          | PromptView
          | ArtistListView
          | SingleImageView
          deriving Eq

data Field = CmdField
  deriving (Ord, Eq, Show)

data Mode = ArtistIllustrations
          | SingleIllustration
          | FollowingArtists
          | SearchArtists
          | FollowingArtistsIllustrations
          | RecommendedIllustrations
          | Info
          | Manual
          | BrowseCache
          | Home
          deriving Eq

data Event = ModeEnter Mode
           | LoginResult (Either String String)
           | DownloadFinished St
           | RequestFinished St

data St =
  St
    {
    -- UI position stuff
      _images :: [String]
    -- ^ images to display (not all of them will fit in screen)
    , _displayedImages :: [String]
    -- ^ currently displayed images
    , _activeView :: View
    -- ^ the currently active view
    , _modeIdx :: Int
    -- ^ the index of the focused/highlighted mode
    -- (not necessarily the currently active mode)
    -- TODO: these should probably in another state type
    -- specific to each mode. there's a need to split up this state record anyway
    , _selectedCellIdx :: Int
    -- ^ idx of selected/highlighted cell
    , _currentPage1 :: Int
    -- ^ **1-indexed** int representing the current page as returned by pixiv.
    -- Pixiv requests are paginated, meaning it will send a limited number
    -- of objects per page, usually 30. To get more objects, you need to request again
    -- using the `next_url` parameter it returns. This field stores the current
    -- pixiv page: in other words, the total_number_of_objects mod 30
    , _currentSlice :: Int
    -- ^ 0-indexed int representing the current slice of the page as defined above.
    -- As it is not physically possible to view all 30 images in a single screen,
    -- every page has to be broken up into sections, or slices. In other words,
    -- this is the scroll position. The total number of slices depends on the
    -- available screen size and the number of rows and columns.
    -- Number of rows * Number of cols * total number of slices = max number of
    -- objects in a single page
    , _offset :: Int

    -- UI stuff
    , _editor :: Editor Text Field
    -- ^ The editor for prompts that ask for artist id or search str
    , _labels :: [Text]
    -- ^ text to display in the second horizontal cell for ArtistListView
    , _history :: [Text]
    -- ^ history of highlighted mode to display
    , _isHistoryFocused :: Bool
    -- ^ Whether the history layer should be focused in welcome view
    , _historyIdx :: Int
    -- ^ The index of the focused history entry
    , _footer :: Widget Field

    -- communication stuff
    , _ub :: Ueberzug
    -- ^ ueberzug process
    , _chan :: BChan Event
    -- ^ the channel to send app events
    --, _socket :: Socket
    , _conn :: Socket
    , _pendingOnLogin :: Maybe (St -> IO St)

    -- config stuff
    , _config :: Config.Types.Config
    , _konekoDir :: FilePath
    -- ^ the koneko dir
    , _ncols_GalleryView :: Int
    , _nrows_GalleryView :: Int
    , _ncols_ArtistListView :: Int
    , _nrows_ArtistListView :: Int
    , _ncols_SingleImageView :: Int
    , _nrows_SingleImageView :: Int

    -- requests
    , _your_id :: Maybe String
    -- ^ the currently logged in user's pixiv id
    }

makeLenses ''St