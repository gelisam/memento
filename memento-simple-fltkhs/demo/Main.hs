module Main where

import MementoSimpleFLTK


view :: () -> SimpleWidget ()
view () = SimpleWidget ()

onEvent :: () -> () -> ()
onEvent () () = ()

main :: IO ()
main = runMementoFLTK () view onEvent
