{-# LANGUAGE OverloadedStrings, RecordWildCards, RecursiveDo #-}
module Main where

import Data.IORef
import Data.Text
import Graphics.UI.FLTK.LowLevel.Fl_Types (Rectangle(..), Position(..), X(..), Y(..), Size(..),Width(..), Height(..))
import qualified Graphics.UI.FLTK.LowLevel.FL as FL
import qualified Graphics.UI.FLTK.LowLevel.Fl_Types as FLTK
import qualified Graphics.UI.FLTK.LowLevel.FLTKHS as FLTK


newButton :: Rectangle -> Text -> IO () -> IO (FLTK.Ref FLTK.Button)
newButton rect label callback = do
  buttonRef <- FLTK.buttonNew rect (Just label)
  FLTK.setCallback buttonRef (\_ -> callback)
  pure buttonRef

newWindow :: Size -> IO (FLTK.Ref FLTK.Window)
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
  , appRefsDirectionButton :: FLTK.Ref FLTK.Button
  , appRefsCountButton     :: FLTK.Ref FLTK.Button
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


newAppWindow :: IO (FLTK.Ref FLTK.Window)
newAppWindow = mdo
  windowRef <- newWindow (Size (Width 130) (Height 90))

  appRefs <- AppRefs <$> newIORef (AppState True 0)
                     <*> newButton (Rectangle (Position (X 30) (Y 30))
                                              (Size (Width 30) (Height 30)))
                                   "^"
                                   (directionButtonCallback appRefs)
                     <*> newButton (Rectangle (Position (X 70) (Y 30))
                                              (Size (Width 30) (Height 30)))
                                   "0"
                                   (countButtonCallback appRefs)
  FLTK.add windowRef (appRefsDirectionButton appRefs)
  FLTK.add windowRef (appRefsCountButton appRefs)

  pure windowRef

main :: IO ()
main = do
  windowRef <- newAppWindow
  FLTK.showWidget windowRef
  _ <- FL.run
  FL.flush
