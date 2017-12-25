{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
module MonoidalDiff where


class Monoid (Patch a) => Diff a where
  type Patch a


instance Diff () where
  type Patch () = ()
