{-# LANGUAGE DeriveFunctor, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TypeFamilies, UndecidableInstances #-}
{-# OPTIONS -Wno-orphans #-}
module MementoSimpleFLTK
 ( module MementoSimpleFLTK
 , Rectangle(..), Position(..), X(..), Y(..), Size(..),Width(..), Height(..)
 ) where

import Control.Applicative
import Control.Monad.IO.Class
import Data.IORef
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

-- Because of Memento's fundeps, this instance is considered an orphan even
-- though PatchButton is defined in this file :(
instance Memento SimpleFLTK ButtonRef (PatchButton (Last Callback)) where
  newMemento (Button rect label callback) = newButton rect label callback
  modifyMemento buttonRef (PatchButton (Last maybeRectangle) (Last maybeLabel) (Last maybeCallback)) = do
    mapM_ (setButtonRectangle  buttonRef) maybeRectangle
    mapM_ (setButtonLabel      buttonRef) maybeLabel
    mapM_ (setButtonCallback   buttonRef) maybeCallback
    pure buttonRef
  deleteMemento = deleteButton


-- Hack to allow two buttons.
-- TODO: allow a variable number of buttons
data Pair a = Pair a a
  deriving (Functor, Show)

instance Applicative Pair where
  pure x = Pair x x
  Pair f1 f2 <*> Pair x1 x2 = Pair (f1 x1) (f2 x2)

instance Foldable Pair where
  foldMap f (Pair x1 x2) = f x1 <> f x2

instance Traversable Pair where
  traverse f (Pair x1 x2) = Pair <$> f x1 <*> f x2

instance Monoid a => Monoid (Pair a) where
  mempty = pure mempty
  mappend = liftA2 mappend

instance Action a => Action (Pair a) where
  type Operand (Pair a) = Pair (Operand a)
  act = liftA2 act

instance Diff a => Diff (Pair a) where
  diff = liftA2 diff

instance Memento m o a => Memento m (Pair o) (Pair a) where
  newMemento = traverse newMemento
  modifyMemento o a = sequenceA $ modifyMemento <$> o <*> a
  deleteMemento = mapM_ deleteMemento

overPair :: (a -> b) -> Pair a -> Pair b
overPair f (Pair x1 x2) = Pair (f x1) (f x2)


runMementoSimpleFLTK :: forall model event. Eq event
                     => model
                     -> (event -> model -> model)
                     -> Size
                     -> (model -> Pair (Button event))
                     -> IO ()
runMementoSimpleFLTK initialModel handler windowSize render = do
  -- initialized below in runSimpleFLTK
  ref <- newIORef (undefined :: (model, Pair (Button event), Pair (Button Callback), Pair ButtonRef))

  let initialEventButtons :: Pair (Button event)
      initialEventButtons = render initialModel

      initialCallbackButtons :: Pair (Button Callback)
      initialCallbackButtons = overPair toCallbackButton initialEventButtons

      toCallbackButton :: Button event -> Button Callback
      toCallbackButton = fmap update

      toPatchCallbackButton :: PatchButton (Last event) -> PatchButton (Last Callback)
      toPatchCallbackButton = (fmap . fmap) update

      update :: event -> SimpleFLTK ()
      update event = do
        (model, eventButtons, callbackButtons, buttonRefs) <- liftIO $ readIORef ref
        let model' = handler event model
            eventButtons' = render model'
            callbackButtons' = act patchCallbackButtons callbackButtons
            patchCallbackButtons = overPair toPatchCallbackButton patchEventButtons
            patchEventButtons = diff eventButtons eventButtons'
        buttonRefs' <- modifyMemento buttonRefs patchCallbackButtons
        liftIO $ writeIORef ref (model', eventButtons', callbackButtons', buttonRefs')

  runSimpleFLTK windowSize $ do
    initialButtonRefs <- newMemento initialCallbackButtons
    liftIO $ writeIORef ref (initialModel, initialEventButtons, initialCallbackButtons, initialButtonRefs)

  -- TODO: Add a finalizer to runSimpleFLTK so we can call deleteMemento at the end
