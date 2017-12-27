{-# LANGUAGE FlexibleContexts, StandaloneDeriving, TypeFamilies #-}
module MonoidalDiff where

import Data.Monoid


-- laws:
--   diff x y `patch` x = y
--   patch mempty = id
--   patch (p <> q) = patch p >>> patch q
--
-- Note that `diff x y <> diff y z = diff x z` is not a law, since we want to
-- allow instances which make use of the extra information about 'y' to infer
-- more precise patches, e.g. a rename and a change of contents instead
-- of a deletion and a creation.
--
-- Also note that there is no guarantee on the meaning of @diff x y@ when
-- applied to values other than @x@. In particular, it is tempting to write
-- @diff x y1 <> diff x y2 `patch` x@ to mimic a version-control system's
-- three-way merge. This will always produce some result, but it cannot be the
-- result you would expect from a three-way merge, because such a merge can
-- fail with a merge conflict, whereas 'patch' has no way to indicate failure.
class Monoid (Patch a) => Diff a where
  type Patch a
  diff  :: a -> a -> Patch a
  patch :: Patch a -> a -> a


instance Diff () where
  type Patch () = ()
  diff () () = ()
  patch () () = ()


newtype Atomic a = Atomic { getAtomic :: a }
  deriving Show

instance Eq a => Diff (Atomic a) where
  type Patch (Atomic a) = Last a
  diff (Atomic x) (Atomic x') | x == x'   = Last $ Nothing
                              | otherwise = Last $ Just x'
  patch (Last Nothing)   (Atomic x) = Atomic x
  patch (Last (Just x')) _          = Atomic x'


instance (Diff a, Diff b) => Diff (a, b) where
  type Patch (a, b) = (Patch a, Patch b)
  diff (a, b) (a', b') = (diff a a', diff b b')
  patch (a2a', b2b') (a, b) = (patch a2a' a, patch b2b' b)


data PatchEither a b
  = PatchEither (Patch a) (Patch b)
  | ReplaceEither (Either a b)
deriving instance (Show a, Show b, Show (Patch a), Show (Patch b)) => Show (PatchEither a b)

instance (Diff a, Diff b) => Monoid (PatchEither a b) where
  mempty = PatchEither mempty mempty
  PatchEither a2a' b2b'   `mappend` PatchEither a'2a'' b'2b'' = PatchEither (a2a' <> a'2a'') (b2b' <> b'2b'')
  ReplaceEither (Left  a) `mappend` PatchEither a2a'   _      = ReplaceEither $ Left  $ patch a2a' a
  ReplaceEither (Right b) `mappend` PatchEither _      b2b'   = ReplaceEither $ Right $ patch b2b' b
  _                       `mappend` ReplaceEither e'          = ReplaceEither e'

instance (Diff a, Diff b) => Diff (Either a b) where
  type Patch (Either a b) = PatchEither a b
  diff (Left  a) (Left  a') = PatchEither (diff a a') mempty
  diff (Right b) (Right b') = PatchEither mempty (diff b b')
  diff _         e'         = ReplaceEither e'
  patch (PatchEither a2a' _) (Left  a) = Left  $ patch a2a' a
  patch (PatchEither _ b2b') (Right b) = Right $ patch b2b' b
  patch (ReplaceEither e')   _         = e'
