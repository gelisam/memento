{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses #-}
module Memento where

import MonoidalDiff


class (Monad m, Action a) => Memento m o a | o -> m a where
  newMemento    :: Operand a -> m o
  modifyMemento :: a -> o -> m o
  deleteMemento :: o -> m ()
