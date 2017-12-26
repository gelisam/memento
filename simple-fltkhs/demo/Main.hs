{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Graphics.UI.FLTK.LowLevel.FL as FL
import qualified Graphics.UI.FLTK.LowLevel.Fl_Types as FLTK
import qualified Graphics.UI.FLTK.LowLevel.FLTKHS as FLTK


buttonCb :: FLTK.Ref FLTK.Button -> IO ()
buttonCb buttonRef = do
  label <- FLTK.getLabel buttonRef
  if (label == "Hello world")
  then FLTK.setLabel buttonRef "Goodbye world"
  else FLTK.setLabel buttonRef "Hello world"

ui :: IO ()
ui = do
  windowRef <- FLTK.windowNew (FLTK.Size (FLTK.Width 115) (FLTK.Height 100))
                              Nothing
                              Nothing
  FLTK.begin windowRef
  do buttonRef <- FLTK.buttonNew (FLTK.Rectangle (FLTK.Position (FLTK.X 10) (FLTK.Y 30))
                                                 (FLTK.Size (FLTK.Width 95) (FLTK.Height 30)))
                                 (Just "Hello world")
     FLTK.setLabelsize buttonRef (FLTK.FontSize 10)
     FLTK.setCallback buttonRef buttonCb
  FLTK.end windowRef
  FLTK.showWidget windowRef

main :: IO ()
main = do
  ui
  _ <- FL.run
  FL.flush
