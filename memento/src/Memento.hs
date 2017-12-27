{-# LANGUAGE FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses, UndecidableInstances #-}
module Memento where

import Data.IORef
import Data.Monoid

import MonoidalDiff


class (Monad m, Action a) => Memento m o a | o -> m a where
  newMemento    :: Operand a -> m o
  modifyMemento :: o -> a -> m o
  deleteMemento :: o -> m ()


instance Memento IO (IORef a) (Last a) where
  newMemento = newIORef
  modifyMemento ref (Last Nothing) = pure ref
  modifyMemento ref (Last (Just x')) = do
    writeIORef ref x'
    pure ref
  deleteMemento _ = pure ()


instance (Memento m o1 a1, Memento m o2 a2) => Memento m (o1, o2) (a1, a2) where
  newMemento (x1, x2) = do
    o1 <- newMemento x1
    o2 <- newMemento x2
    pure (o1, o2)
  modifyMemento (o1, o2) (a1, a2) = do
    o1' <- modifyMemento o1 a1
    o2' <- modifyMemento o2 a2
    pure (o1', o2')
  deleteMemento (o1, o2) = do
    deleteMemento o1
    deleteMemento o2


instance (Memento m o1 a1, Memento m o2 a2) => Memento m (Either o1 o2) (PatchEither a1 a2) where
  newMemento (Left  x) = Left  <$> newMemento x
  newMemento (Right y) = Right <$> newMemento y
  modifyMemento (Left  o1) (PatchEither a1 _) = Left  <$> modifyMemento o1 a1
  modifyMemento (Right o2) (PatchEither _ a2) = Right <$> modifyMemento o2 a2
  modifyMemento o          (ReplaceEither x)  = do
    deleteMemento o
    newMemento x
  deleteMemento (Left  o1) = deleteMemento o1
  deleteMemento (Right o2) = deleteMemento o2
