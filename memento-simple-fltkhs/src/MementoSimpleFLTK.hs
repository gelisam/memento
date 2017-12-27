{-# LANGUAGE DeriveFunctor, FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}
{-# OPTIONS -Wno-orphans #-}
module MementoSimpleFLTK where

import Data.Monoid

import Memento
import MonoidalDiff
import SimpleFLTK


data SimpleWidget event = SimpleWidget event
  deriving (Functor, Show)
data PatchSimpleWidget patchEvent = PatchSimpleWidget patchEvent
  deriving (Functor, Show)

instance Monoid patchEvent => Monoid (PatchSimpleWidget patchEvent) where
  mempty = PatchSimpleWidget mempty
  PatchSimpleWidget p `mappend` PatchSimpleWidget p' = PatchSimpleWidget (p <> p')

instance Action patchEvent => Action (PatchSimpleWidget patchEvent) where
  type Operand (PatchSimpleWidget patchEvent) = SimpleWidget (Operand patchEvent)
  act (PatchSimpleWidget p) = fmap (act p)

instance Diff patchEvent => Diff (PatchSimpleWidget patchEvent) where
  diff (SimpleWidget e) (SimpleWidget e') = PatchSimpleWidget (diff e e')

--- Because of Memento's fundeps, this instance considered an orphan even though
--- PatchSimpleWidget is defined in this file :(
instance Memento IO SimpleWidgetO (PatchSimpleWidget (Last (IO ()))) where
  newMemento (SimpleWidget callback) = pure $ SimpleWidgetO callback
  modifyMemento (PatchSimpleWidget p) (SimpleWidgetO callback) = pure $ SimpleWidgetO (act p callback)
  deleteMemento _ = pure ()


runMementoFLTK :: (Action model, Eq event)
               => model
               -> (model -> SimpleWidget event)
               -> (event -> model -> model)
               -> IO ()
runMementoFLTK _ _ _ = pure ()
