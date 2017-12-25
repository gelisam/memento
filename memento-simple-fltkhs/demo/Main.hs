module Main where

import MementoSimpleFLTK
import MonoidalDiff


view :: () -> SimpleWidget ()
view () = SimpleWidget

onEvent :: () -> Patch ()
onEvent () = ()

main :: IO ()
main = runMementoFLTK () view onEvent
