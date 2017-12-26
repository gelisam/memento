{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}
{-# OPTIONS -Wno-orphans #-}
module MementoSimpleFLTK where

import Memento
import MonoidalDiff
import SimpleFLTK


data SimpleWidget event = SimpleWidget

instance Diff (SimpleWidget event) where
  type Patch (SimpleWidget event) = ()
  diff SimpleWidget SimpleWidget = ()
  patch () SimpleWidget = SimpleWidget

-- because of Memento's fundeps, thisInstance considered an orphan even though
-- SimpleWidget is defined in this file :(
instance Memento IO SimpleWidgetO (SimpleWidget (IO ())) where
  newMemento SimpleWidget = pure SimpleWidgetO
  modifyMemento () SimpleWidgetO = pure SimpleWidgetO
  deleteMemento SimpleWidgetO = pure ()


runMementoFLTK :: (Diff model, Eq event)
               => model
               -> (model -> SimpleWidget event)
               -> (event -> Patch model)
               -> IO ()
runMementoFLTK _ _ _ = pure ()
