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
import Serialization.In (IPCResponses, IPCResponse)
import qualified Data.IntMap.Lazy as Lazy
import qualified Data.IntMap.Strict as Strict

data View = GalleryView
          | WelcomeView
          | PromptView
          | ArtistListView
          | PostView
          deriving (Eq, Show)

data Field = CmdField
  deriving (Ord, Eq, Show)

data Mode = ArtistIllustrations
          | PixivPost
          | FollowingArtists
          | SearchArtists
          | FollowingArtistsIllustrations
          | RecommendedIllustrations
          deriving (Eq, Show)

data Event = ModeEnter Mode
           | ReturnToHome
           | LoginResult (Either String String)
           | UpdateSt St
           | IPCReceived IPCResponse

data Request =
  Request
    { labels_ :: [String]
    -- ^ text to display in the second horizontal cell for ArtistListView
    , paths :: [String]
    -- ^ images to display (not all of them will fit in screen)
    , urls :: [String]
    , nextUrl_ :: Maybe String
    , image_ids :: [Maybe Int]
    -- ^ image ids for artworks for open in browser
    , original_urls :: [Maybe String]
    -- ^ original resolution url for downloading
    }
    deriving Show

data St =
  St
    {
    -- UI position stuff
    _displayedImages :: [String]
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
    , _messageQueue :: Strict.IntMap (IPCResponses -> IO ())

    -- config stuff
    , _config :: Config
    -- ^ the config file
    , _konekoDir :: FilePath
    -- ^ the koneko dir

    -- requests
    , _requestsCache1 :: Lazy.IntMap Request
    -- ^ All the parsed requests for every page for this mode,
    -- indexed by _currentPage1 (so it's 1-indexed as well)
    , _your_id :: Maybe String
    -- ^ the currently logged in user's pixiv id
    }

makeLenses ''St
