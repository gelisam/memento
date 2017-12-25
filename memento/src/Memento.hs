{-# LANGUAGE MultiParamTypeClasses #-}
module Memento where

import MonoidalDiff


class (Monad m, Diff a) => Memento m o a
