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


type SimpleFLTK = ReaderT Window IO

runSimpleFLTK :: Size -> SimpleFLTK () -> IO ()
runSimpleFLTK windowSize body = do
  window <- FLTK.windowNew windowSize Nothing Nothing
  runReaderT body window
  FLTK.showWidget window
  _ <- FL.run
  FL.flush


newButton :: Rectangle -> Text -> IO () -> SimpleFLTK Button
newButton rect label callback = ReaderT $ \window -> do
  button <- FLTK.buttonNew rect (Just label)
  FLTK.setCallback button (\_ -> callback)

  FLTK.add window button
  pure button

modifyButtonLabel :: Button -> Text -> SimpleFLTK ()
modifyButtonLabel button label = liftIO $ FLTK.setLabel button label

modifyButtonCallback :: Button -> IO () -> SimpleFLTK ()
modifyButtonCallback button callback = liftIO $ FLTK.setCallback button (\_ -> callback)

deleteButton :: Button -> SimpleFLTK ()
deleteButton button = ReaderT $ \window -> do
  FLTK.removeWidget window button
  FL.deleteWidget button


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


main :: IO ()
main = runSimpleFLTK (Size (Width 130) (Height 90)) $ mdo
  appRefs <- AppRefs <$> liftIO (newIORef (AppState True 0))
                     <*> newButton (Rectangle (Position (X 30) (Y 30))
                                              (Size (Width 30) (Height 30)))
                                   "^"
                                   (directionButtonCallback appRefs)
                     <*> newButton (Rectangle (Position (X 70) (Y 30))
                                              (Size (Width 30) (Height 30)))
                                   "0"
                                   (countButtonCallback appRefs)
  pure ()
