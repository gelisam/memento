{-# LANGUAGE OverloadedStrings, RecursiveDo #-}
module Main where

import Data.IORef
import Data.Text
import Graphics.UI.FLTK.LowLevel.Fl_Types (Rectangle(..), Position(..), X(..), Y(..), Size(..),Width(..), Height(..))
import qualified Graphics.UI.FLTK.LowLevel.FL as FL
import qualified Graphics.UI.FLTK.LowLevel.Fl_Types as FLTK
import qualified Graphics.UI.FLTK.LowLevel.FLTKHS as FLTK


buttonCb :: IORef Bool -> FLTK.Ref FLTK.Button -> IO ()
buttonCb boolRef buttonRef = do
  modifyIORef boolRef not
  bool <- readIORef boolRef
  if bool
  then FLTK.setLabel buttonRef "Goodbye world"
  else FLTK.setLabel buttonRef "Hello world"

newButton :: Rectangle -> Text -> IO () -> IO (FLTK.Ref FLTK.Button)
newButton rect label callback = do
  buttonRef <- FLTK.buttonNew rect (Just label)
  FLTK.setCallback buttonRef (\_ -> callback)
  pure buttonRef

newWindow :: Size -> IO (FLTK.Ref FLTK.Window)
newWindow size = FLTK.windowNew size Nothing Nothing

newAppWindow :: IO (FLTK.Ref FLTK.Window)
newAppWindow = mdo
  boolRef <- newIORef False
  windowRef <- newWindow (Size (Width 115) (Height 100))

  buttonRef <- newButton (Rectangle (Position (X 10) (Y 30))
                                    (Size (Width 95) (Height 30)))
                         "Hello world"
                         (buttonCb boolRef buttonRef)
  FLTK.setLabelsize buttonRef (FLTK.FontSize 10)
  FLTK.add windowRef buttonRef

  pure windowRef

main :: IO ()
main = do
  windowRef <- newAppWindow
  FLTK.showWidget windowRef
  _ <- FL.run
  FL.flush
