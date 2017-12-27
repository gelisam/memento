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


data SimpleWidgetO = SimpleWidgetO (IO ())


type Window = FLTK.Ref FLTK.Window
type Button = FLTK.Ref FLTK.Button


type SimpleFLTK = ReaderT Window IO

runSimpleFLTK :: Size -> SimpleFLTK () -> IO ()
runSimpleFLTK windowSize body = do
  window <- FLTK.windowNew windowSize Nothing Nothing
  runReaderT body window
  FLTK.showWidget window
  _ <- FL.run
  FL.flush


newButton :: Rectangle -> Text -> IO () -> SimpleFLTK Button
newButton rect label callback = ReaderT $ \window -> do
  button <- FLTK.buttonNew rect (Just label)
  FLTK.setCallback button (\_ -> callback)

  FLTK.add window button
  pure button

modifyButtonLabel :: Button -> Text -> SimpleFLTK ()
modifyButtonLabel button label = liftIO $ FLTK.setLabel button label

modifyButtonCallback :: Button -> IO () -> SimpleFLTK ()
modifyButtonCallback button callback = liftIO $ FLTK.setCallback button (\_ -> callback)

deleteButton :: Button -> SimpleFLTK ()
deleteButton button = ReaderT $ \window -> do
  FLTK.removeWidget window button
  FL.deleteWidget button
