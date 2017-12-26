{-# LANGUAGE OverloadedStrings, RecursiveDo #-}
module Main where

import Data.Text
import Graphics.UI.FLTK.LowLevel.Fl_Types (Rectangle(..), Position(..), X(..), Y(..), Size(..),Width(..), Height(..))
import qualified Graphics.UI.FLTK.LowLevel.FL as FL
import qualified Graphics.UI.FLTK.LowLevel.Fl_Types as FLTK
import qualified Graphics.UI.FLTK.LowLevel.FLTKHS as FLTK


buttonCb :: FLTK.Ref FLTK.Button -> IO ()
buttonCb buttonRef = do
  label <- FLTK.getLabel buttonRef
  if (label == "Hello world")
  then FLTK.setLabel buttonRef "Goodbye world"
  else FLTK.setLabel buttonRef "Hello world"

newButton :: Rectangle -> Text -> IO () -> IO (FLTK.Ref FLTK.Button)
newButton rect label callback = do
  buttonRef <- FLTK.buttonNew rect (Just label)
  FLTK.setCallback buttonRef (\_ -> callback)
  pure buttonRef

newAppWindow :: IO (FLTK.Ref FLTK.Window)
newAppWindow = mdo
  windowRef <- FLTK.windowNew (Size (Width 115) (Height 100))
                              Nothing
                              Nothing

  buttonRef <- newButton (Rectangle (Position (X 10) (Y 30))
                                    (Size (Width 95) (Height 30)))
                         "Hello world"
                         (buttonCb buttonRef)
  FLTK.setLabelsize buttonRef (FLTK.FontSize 10)
  FLTK.add windowRef buttonRef

  pure windowRef

main :: IO ()
main = do
  windowRef <- newAppWindow
  FLTK.showWidget windowRef
  _ <- FL.run
  FL.flush
