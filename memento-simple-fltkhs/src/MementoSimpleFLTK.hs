{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}
module MementoSimpleFLTK where

import Memento
import MonoidalDiff


data SimpleWidgetO = SimpleWidgetO
data SimpleWidget event = SimpleWidget

instance Diff (SimpleWidget event) where
  type Patch (SimpleWidget event) = ()

instance Memento IO SimpleWidgetO (SimpleWidget (IO ()))


runMementoFLTK :: (Diff model, Eq event)
               => model
               -> (model -> SimpleWidget event)
               -> (event -> Patch model)
               -> IO ()
runMementoFLTK _ _ _ = pure ()
