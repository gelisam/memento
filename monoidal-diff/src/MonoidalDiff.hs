{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
module MonoidalDiff where


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
