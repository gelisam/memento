{-# LANGUAGE OverloadedStrings, RecordWildCards, RecursiveDo #-}
module Main where

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.IORef
import Data.Text
import Graphics.UI.FLTK.LowLevel.Fl_Types (Rectangle(..), Position(..), X(..), Y(..), Size(..),Width(..), Height(..))
import qualified Graphics.UI.FLTK.LowLevel.FL as FL
import qualified Graphics.UI.FLTK.LowLevel.Fl_Types as FLTK
import qualified Graphics.UI.FLTK.LowLevel.FLTKHS as FLTK


type Window = FLTK.Ref FLTK.Window
type Button = FLTK.Ref FLTK.Button
type M = ReaderT Window IO

newButton :: Rectangle -> Text -> IO () -> M Button
newButton rect label callback = ReaderT $ \window -> do
  button <- FLTK.buttonNew rect (Just label)
  FLTK.setCallback button (\_ -> callback)

  FLTK.add window button
  pure button

modifyButtonLabel :: Button -> Text -> M ()
modifyButtonLabel button label = liftIO $ FLTK.setLabel button label

modifyButtonCallback :: Button -> IO () -> M ()
modifyButtonCallback button callback = liftIO $ FLTK.setCallback button (\_ -> callback)

deleteButton :: Button -> M ()
deleteButton button = ReaderT $ \window -> do
  FLTK.removeWidget window button
  FL.deleteWidget button


newWindow :: Size -> IO Window
newWindow size = FLTK.windowNew size Nothing Nothing


data AppState = AppState
  { appStateGoingUp :: Bool
  , appStateCount   :: Int
  }
  deriving Show

toggleDirection :: AppState -> AppState
toggleDirection appState@AppState{..} = appState
                                      { appStateGoingUp = not appStateGoingUp }

bumpCount :: AppState -> AppState
bumpCount appState@AppState{..} = appState
                                { appStateCount = if appStateGoingUp
                                                  then appStateCount + 1
                                                  else appStateCount - 1
                                }


data AppRefs = AppRefs
  { appRefsAppState        :: IORef AppState
  , appRefsDirectionButton :: Button
  , appRefsCountButton     :: Button
  }

refresh :: AppRefs -> IO ()
refresh appRefs = do
  AppState{..} <- readIORef (appRefsAppState appRefs)
  FLTK.setLabel (appRefsDirectionButton appRefs) $ if appStateGoingUp then "^" else "v"
  FLTK.setLabel (appRefsCountButton     appRefs) $ pack . show $ appStateCount

directionButtonCallback :: AppRefs -> IO ()
directionButtonCallback appRefs = do
  modifyIORef (appRefsAppState appRefs) toggleDirection
  refresh appRefs

countButtonCallback :: AppRefs -> IO ()
countButtonCallback appRefs = do
  modifyIORef (appRefsAppState appRefs) bumpCount
  refresh appRefs


newAppWindow :: IO Window
newAppWindow = mdo
  window <- newWindow (Size (Width 130) (Height 90))
  appRefs <- flip runReaderT window $ do
    AppRefs <$> liftIO (newIORef (AppState True 0))
            <*> newButton (Rectangle (Position (X 30) (Y 30))
                                     (Size (Width 30) (Height 30)))
                          "^"
                          (directionButtonCallback appRefs)
            <*> newButton (Rectangle (Position (X 70) (Y 30))
                                     (Size (Width 30) (Height 30)))
                          "0"
                          (countButtonCallback appRefs)
  pure window

main :: IO ()
main = do
  window <- newAppWindow
  FLTK.showWidget window
  _ <- FL.run
  FL.flush
