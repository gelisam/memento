{-# LANGUAGE DeriveFunctor, FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}
{-# OPTIONS -Wno-orphans #-}
module MementoSimpleFLTK where

import Data.Monoid
import Data.Text

import Memento
import MonoidalDiff
import SimpleFLTK


data Button event = Button
  { buttonRectangle :: Rectangle
  , buttonLabel     :: Text
  , buttonEvent     :: event
  }
  deriving (Functor, Show)

data PatchButton patchEvent = PatchButton
  { patchButtonRectangle :: Last Rectangle
  , patchButtonLabel     :: Last Text
  , patchButtonEvent     :: patchEvent
  }
  deriving (Functor, Show)

instance Monoid patchEvent => Monoid (PatchButton patchEvent) where
  mempty
    = PatchButton mempty mempty mempty
  PatchButton p1 p2 p3 `mappend` PatchButton p1' p2' p3'
    = PatchButton (p1 <> p1') (p2 <> p2') (p3 <> p3')

instance Action patchEvent => Action (PatchButton patchEvent) where
  type Operand (PatchButton patchEvent) = Button (Operand patchEvent)
  act (PatchButton p1 p2 p3) (Button x1 x2 x3)
    = Button (act p1 x1) (act p2 x2) (act p3 x3)

instance Diff patchEvent => Diff (PatchButton patchEvent) where
  diff (Button x1 x2 x3) (Button x1' x2' x3')
    = PatchButton (diff x1 x1') (diff x2 x2') (diff x3 x3')

--- Because of Memento's fundeps, this instance considered an orphan even though
--- PatchButton is defined in this file :(
instance Memento SimpleFLTK ButtonRef (PatchButton (Last Callback)) where
  newMemento (Button rect label callback) = newButton rect label callback
  modifyMemento buttonRef (PatchButton (Last maybeRectangle) (Last maybeLabel) (Last maybeCallback)) = do
    mapM_ (setButtonRectangle  buttonRef) maybeRectangle
    mapM_ (setButtonLabel      buttonRef) maybeLabel
    mapM_ (setButtonCallback   buttonRef) maybeCallback
    pure buttonRef
  deleteMemento = deleteButton


runMementoFLTK :: (Action model, Eq event)
               => model
               -> (model -> Button event)
               -> (event -> model -> model)
               -> IO ()
runMementoFLTK _ _ _ = pure ()
