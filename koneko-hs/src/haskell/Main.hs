{-# LANGUAGE OverloadedStrings #-}

module Main where

import Graphics ( onAppStart )
import Types
import Serialization.In
    ( LoginResponse(_LoginResponse_user), LoginUser(_LoginUser_id), IPCResponses (LoginInfo) )
import Events ( appEvent )
import UI ( drawUI )
import Lens.Micro ((^.))
import qualified Graphics.Vty as V
import Brick.Main
  ( App(..)
  , showFirstCursor
  , customMain
  )
import Brick.AttrMap
  ( attrMap
  )

import Graphics.Ueberzug
    ( clear,
      newUeberzug )
import System.Directory (getHomeDirectory, removePathForcibly)
import qualified Brick.Widgets.Edit as E
import Brick.BChan (newBChan, writeBChan)
import System.FilePath ((</>))
import Network.Socket
    ( maxListenQueue,
      withSocketsDo,
      accept,
      bind,
      listen,
      socket,
      close,
      Family(AF_UNIX),
      SockAddr(SockAddrUnix),
      Socket,
      SocketType(Stream) )
import System.Process (proc, createProcess, cleanupProcess)
import Data.Aeson (eitherDecodeFileStrict)
import Brick (fg)
import Graphics.Vty ( green, red, bold )
import Graphics.Vty.Attributes (blue, withStyle)
import Control.Exception (bracket)
import Config.Types ( Config, pythonProcessPath, refreshToken )
import Requests (login)
import Control.Concurrent (forkIO)
import Control.Monad (void)
import Common (initialFooter, logger)
import Data.IntMap.Lazy (empty)
import Sockets (recvAll)

initialState ub' chan config konekoDir conn =
  St { _selectedCellIdx = 0
     , _ub = ub'
     , _displayedImages = []
     , _currentPage1 = 1
     , _currentSlice = 0
     , _offset = 0
     , _activeView = WelcomeView
     , _modeIdx = 0
     , _editor = E.editor CmdField (Just 1) ""
     , _history = []
     , _isHistoryFocused = False
     , _historyIdx = 0
     , _chan = chan
     , _footer = initialFooter
     , _config = config
     , _konekoDir = konekoDir
     , _conn = conn
     , _pendingOnLogin = Nothing
     , _your_id = Nothing
     , _request = empty
     , _messageQueue = empty
     }

theMap = attrMap V.defAttr
  [ ("input" <> "valid", fg green)
  , ("input" <> "invalid", fg red)
  , ("border" <> "focused", fg blue)
  , ("shortcut", withStyle mempty bold)
  ]

theApp =
  App { appDraw = drawUI
      , appChooseCursor = showFirstCursor
      , appHandleEvent = appEvent
      , appStartEvent = onAppStart
      , appAttrMap = const theMap
      }

main :: IO ()
main = do
  removePathForcibly "/tmp/test_sock.ipc"
  sock <- mksocket
  home <- getHomeDirectory
  let config_path = home </> ".config" </> "koneko_new" </> "config.json"
  e_config <- (eitherDecodeFileStrict config_path :: IO (Either String Config))
  let config =
        case e_config of
          Left x -> error x
          Right x -> x
  let path = config^.pythonProcessPath
  bracket
    (createProcess (proc "python" [path]))
    -- it doesn't seem to clean the process up when haskell program is killed
    -- but it does close the socket, and the python program can be set to exit
    -- when the socket is closed
    (\a -> cleanupProcess a *> close sock)
    (const (main' home config sock))

main' :: [Char] -> Config -> Socket -> IO ()
main' home config sock = do
  (conn, _) <- accept sock

  ub' <- newUeberzug
  let konekoDir = home </> ".local/share/koneko/cache"

  chan <- newBChan 10

  void $ forkIO $
    recvAll chan conn

  let st = initialState ub' chan config konekoDir conn
  void $ forkIO $ do
    new_st <- login (config^.refreshToken) st $ \ir -> do
      res <-
        case ir of
          LoginInfo lr -> do
            let e_id = _LoginUser_id $ _LoginResponse_user lr
            pure (LoginResult $ Right e_id)
          _ -> pure (LoginResult $ Left "Wrong response type")
      writeBChan chan res
    case new_st of
      Left _ -> pure ()
      Right x -> writeBChan chan (RequestFinished x)

  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  let app = theApp
  _final_state <- customMain initialVty buildVty (Just chan) app st
  --threadDelay 1000000
  -- _ <- clear (_final_state^.ub) "crab"
  --threadDelay 1000000

  --close conn
  --close sock
  pure ()

mksocket :: IO Socket
mksocket =
  withSocketsDo $ do
    sock <- socket AF_UNIX Stream 0
    bind sock (SockAddrUnix "/tmp/test_sock.ipc")
    listen sock maxListenQueue
    pure sock
