module SimpleFLTK
 ( module SimpleFLTK
 , Rectangle(..), Position(..), X(..), Y(..), Size(..),Width(..), Height(..)
 ) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Text
import Graphics.UI.FLTK.LowLevel.Fl_Types (Rectangle(..), Position(..), X(..), Y(..), Size(..),Width(..), Height(..))
import qualified Graphics.UI.FLTK.LowLevel.FL as FL
import qualified Graphics.UI.FLTK.LowLevel.Fl_Types as FLTK
import qualified Graphics.UI.FLTK.LowLevel.FLTKHS as FLTK


type WindowRef = FLTK.Ref FLTK.Window
type ButtonRef = FLTK.Ref FLTK.Button


type SimpleFLTK = ReaderT WindowRef IO
type Callback = SimpleFLTK ()

runSimpleFLTK :: Size -> SimpleFLTK () -> IO ()
runSimpleFLTK windowSize body = do
  windowRef <- FLTK.windowNew windowSize Nothing Nothing
  runReaderT body windowRef
  FLTK.showWidget windowRef
  _ <- FL.run
  FL.flush


newButton :: Rectangle -> Text -> Callback -> SimpleFLTK ButtonRef
newButton rect label callback = ReaderT $ \windowRef -> do
  buttonRef <- FLTK.buttonNew rect (Just label)
  FLTK.setCallback buttonRef (\_ -> runReaderT callback windowRef)

  FLTK.add windowRef buttonRef
  pure buttonRef

setButtonRectangle :: ButtonRef -> Rectangle -> SimpleFLTK ()
setButtonRectangle buttonRef rectangle = liftIO $ FLTK.resize buttonRef rectangle

setButtonLabel :: ButtonRef -> Text -> SimpleFLTK ()
setButtonLabel buttonRef label = liftIO $ FLTK.setLabel buttonRef label

setButtonCallback :: ButtonRef -> Callback -> SimpleFLTK ()
setButtonCallback buttonRef callback = ReaderT $ \windowRef -> do
  FLTK.setCallback buttonRef (\_ -> runReaderT callback windowRef)

deleteButton :: ButtonRef -> SimpleFLTK ()
deleteButton buttonRef = ReaderT $ \windowRef -> do
  FLTK.removeWidget windowRef buttonRef
  FL.deleteWidget buttonRef
