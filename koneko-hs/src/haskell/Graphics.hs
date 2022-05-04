module Graphics where

import Graphics.Ueberzug
    ( defaultUbConf,
      draw,
      UbConf(identifier, path, x, y, width, height) )
import System.FilePath ((</>), takeDirectory)
import Lens.Micro ( (&), (.~), (^.) )
import Types
    ( displayedImages,
      history,
      konekoDir,
      ub,
      St, )
import Control.Monad.IO.Class (MonadIO(liftIO))
import qualified Data.Text.IO as T
import History (parseHistoryFile)

radioInner :: St -> IO St
radioInner st = do
  file <- T.readFile (st^.konekoDir </> "history_new")
  pure $ st & history .~ (fst <$> parseHistoryFile st file)

onAppStart :: MonadIO m => St -> m St
onAppStart st = liftIO (radioInner st >>= displayHomeImage)

displayHomeImage :: St -> IO St
displayHomeImage st = do
  Right () <-
    draw (st^.ub) $ defaultUbConf
      { identifier = "home"
      , path = takeDirectory (st^.konekoDir) </> "pics/71471144_p0.png"
      , x = 1
      , y = 1
      , width = Just 28
      , height = Just 28
      }
  pure $ st & displayedImages .~ ["home"]
