{-# LANGUAGE FlexibleContexts, StandaloneDeriving, TypeFamilies #-}
module MonoidalDiff where

import Control.Applicative
import Data.Monoid


-- laws:
--   act mempty = id
--   act (p <> q) = act p >>> act q
--
class Monoid a => Action a where
  type Operand a
  act :: a -> Operand a -> Operand a

-- laws:
--   diff x y `act` x = y
--
-- Note that `diff x y <> diff y z = diff x z` is not a law, since we want to
-- allow instances which make use of the extra information about 'y' to infer
-- more precise patches, e.g. a rename and a change of contents instead
-- of a deletion and a creation.
--
-- Also note that there is no guarantee on the meaning of @diff x y@ when
-- applied to values other than @x@. In particular, it is tempting to write
-- @diff x y1 <> diff x y2 `act` x@ to mimic a version-control system's
-- three-way merge. This will always produce some result, but it cannot be the
-- result you would expect from a three-way merge, because such a merge can
-- fail with a merge conflict, whereas 'act' has no way to indicate failure.
class Action a => Diff a where
  diff  :: Operand a -> Operand a -> a


instance Action () where
  type Operand () = ()
  act () () = ()

instance Diff () where
  diff () () = ()


instance (Action a, Action b) => Action (a, b) where
  type Operand (a, b) = (Operand a, Operand b)
  act (a, b) (x, y) = (act a x, act b y)

instance (Diff a, Diff b) => Diff (a, b) where
  diff (x, y) (x', y') = (diff x x', diff y y')


data PatchEither a b
  = PatchEither a b
  | ReplaceEither (Either (Operand a) (Operand b))
deriving instance (Show a, Show b, Show (Operand a), Show (Operand b)) => Show (PatchEither a b)

instance (Action a, Action b) => Monoid (PatchEither a b) where
  mempty = PatchEither mempty mempty
  PatchEither a b `mappend` PatchEither a' b' = PatchEither (a <> a') (b <> b')
  _               `mappend` ReplaceEither x'  = ReplaceEither x'
  ReplaceEither x `mappend` p                 = ReplaceEither $ act p x

instance (Action a, Action b) => Action (PatchEither a b) where
  type Operand (PatchEither a b) = Either (Operand a) (Operand b)
  act (PatchEither a _) (Left  x) = Left  (act a x)
  act (PatchEither _ b) (Right y) = Right (act b y)
  act (ReplaceEither x) _         = x

instance (Diff a, Diff b) => Diff (PatchEither a b) where
  diff (Left  x) (Left  x') = PatchEither (diff x x') mempty
  diff (Right y) (Right y') = PatchEither mempty (diff y y')
  diff _         x'         = ReplaceEither x'


instance Action All where
  type Operand All = Bool
  act (All x) = (&& x)


instance Alternative f => Action (Alt f a) where
  type Operand (Alt f a) = f a
  act (Alt x) = (<|> x)


instance Action Any where
  type Operand Any = Bool
  act (Any x) = (|| x)


instance Action (Endo a) where
  type Operand (Endo a) = a
  act (Endo f) = f


instance Action (Last a) where
  type Operand (Last a) = a
  act (Last Nothing)   x = x
  act (Last (Just x')) _ = x'

instance Eq a => Diff (Last a) where
  diff x x' | x == x'   = Last $ Nothing
            | otherwise = Last $ Just x'


instance Num a => Action (Product a) where
  type Operand (Product a) = a
  act (Product x) = (* x)

instance Fractional a => Diff (Product a) where
  diff x x' = Product (x' / x)


instance Num a => Action (Sum a) where
  type Operand (Sum a) = a
  act (Sum x) = (+ x)

instance Num a => Diff (Sum a) where
  diff x x' = Sum (x' - x)
