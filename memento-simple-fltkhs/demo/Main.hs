{-# LANGUAGE OverloadedStrings #-}
module Main where

import MementoSimpleFLTK
import SimpleFLTK


view :: () -> Button ()
view () = Button (Rectangle (Position (X 0) (Y 0))
                            (Size (Width 100) (Height 100)))
                 "hello"
                 ()

onEvent :: () -> () -> ()
onEvent () () = ()

main :: IO ()
main = runMementoFLTK () view onEvent
