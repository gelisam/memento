{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Graphics.UI.FLTK.LowLevel.FL as FL
import Graphics.UI.FLTK.LowLevel.Fl_Types
import Graphics.UI.FLTK.LowLevel.FLTKHS


buttonCb :: Ref Button -> IO ()
buttonCb buttonRef = do
  label <- getLabel buttonRef
  if (label == "Hello world")
  then setLabel buttonRef "Goodbye world"
  else setLabel buttonRef "Hello world"

ui :: IO ()
ui = do
  windowRef <- windowNew (Size (Width 115) (Height 100))
                         Nothing
                         Nothing
  begin windowRef
  do buttonRef <- buttonNew (Rectangle (Position (X 10) (Y 30))
                                       (Size (Width 95) (Height 30)))
                            (Just "Hello world")
     setLabelsize buttonRef (FontSize 10)
     setCallback buttonRef buttonCb
  end windowRef
  showWidget windowRef

main :: IO ()
main = do
  ui
  _ <- FL.run
  FL.flush
