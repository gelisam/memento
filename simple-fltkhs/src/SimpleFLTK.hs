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


data SimpleWidgetO = SimpleWidgetO Callback


type Window = FLTK.Ref FLTK.Window
type Button = FLTK.Ref FLTK.Button


type SimpleFLTK = ReaderT Window IO
type Callback = SimpleFLTK ()

runSimpleFLTK :: Size -> SimpleFLTK () -> IO ()
runSimpleFLTK windowSize body = do
  window <- FLTK.windowNew windowSize Nothing Nothing
  runReaderT body window
  FLTK.showWidget window
  _ <- FL.run
  FL.flush


newButton :: Rectangle -> Text -> Callback -> SimpleFLTK Button
newButton rect label callback = ReaderT $ \window -> do
  button <- FLTK.buttonNew rect (Just label)
  FLTK.setCallback button (\_ -> runReaderT callback window)

  FLTK.add window button
  pure button

setButtonLabel :: Button -> Text -> SimpleFLTK ()
setButtonLabel button label = liftIO $ FLTK.setLabel button label

setButtonCallback :: Button -> Callback -> SimpleFLTK ()
setButtonCallback button callback = ReaderT $ \window -> do
  FLTK.setCallback button (\_ -> runReaderT callback window)

deleteButton :: Button -> SimpleFLTK ()
deleteButton button = ReaderT $ \window -> do
  FLTK.removeWidget window button
  FL.deleteWidget button
