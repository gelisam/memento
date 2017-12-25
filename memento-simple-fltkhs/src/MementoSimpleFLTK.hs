{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}
module MementoSimpleFLTK where

import Memento
import MonoidalDiff


data SimpleWidgetO = SimpleWidgetO
data SimpleWidget event = SimpleWidget

instance Diff (SimpleWidget event) where
  type Patch (SimpleWidget event) = ()
  diff SimpleWidget SimpleWidget = ()
  patch () SimpleWidget = SimpleWidget

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
