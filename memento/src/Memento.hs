{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses #-}
module Memento where

import MonoidalDiff


class (Monad m, Diff a) => Memento m o a | o -> m a where
  newMemento    :: a -> m o
  modifyMemento :: Patch a -> o -> m o
  deleteMemento :: o -> m ()
