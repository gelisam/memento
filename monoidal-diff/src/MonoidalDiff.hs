{-# LANGUAGE FlexibleContexts, FlexibleInstances, StandaloneDeriving, TypeFamilies #-}
module MonoidalDiff where

import Control.Applicative
import Data.Bifunctor
import Data.Monoid
import Test.QuickCheck

-- $setup
-- >>> :set -XFlexibleInstances
-- >>> :set -XTypeApplications
-- >>> import Control.Arrow ((>>>))
-- >>> import Data.Ratio
--
-- QuickCheck requires a 'Show' instance, but it won't be used
-- since the test passes.
--
-- >>> instance Show (Endo Int) where show (Endo f) = "fmap f [0..10] = " <> show (fmap f [0..10])
-- >>> instance Show (     Int ->      Int) where show f = "fmap f [0..10] = " <> show (fmap f [0..10])
-- >>> instance Show (Sum  Int -> Sum  Int) where show f = "fmap (f . Sum) [0..10] = " <> show (fmap (f . Sum) [0..10])
-- >>> instance Show (Last Int -> Last Int) where show f = "fmap (f . Last . Just) [0..10] = " <> show (fmap (f . Last . Just) [0..10])


-- laws:
--   act mempty = id
--   act (p <> q) = act p >>> act q
--
class Monoid a => Action a where
  type Operand a
  act :: a -> Operand a -> Operand a

-- laws:
--   mapAction id id = id
--   mapAction f f' . mapAction g g' = mapAction (f . g) (f' . g')
class ActionFunctor f where
  mapAction :: (a -> b) -> (Operand a -> Operand b)
            -> f a -> f b

-- laws:
--   bimapAction id id id id = id
--   bimapAction f1 f1' f2 f2' . mapAction g1 g1' g2 g2' = mapAction (f1 . g1) (f1' . g1') (f2 . g2) (f2' . g2')
class ActionBifunctor f where
  bimapAction :: (a1 -> b1) -> (Operand a1 -> Operand b1)
              -> (a2 -> b2) -> (Operand a2 -> Operand b2)
              -> f a1 a2 -> f b1 b2

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


-- |
-- prop> act @() mempty   x == id                x
-- prop> act @() (p <> q) x == (act p >>> act q) x
instance Action () where
  type Operand () = ()
  act () () = ()

-- |
-- prop> diff @() x y `act` x == y
instance Diff () where
  diff () () = ()


-- |
-- prop> act @(Sum Int, Sum Int) mempty   x == id                x
-- prop> act @(Sum Int, Sum Int) (p <> q) x == (act p >>> act q) x
instance (Action a, Action b) => Action (a, b) where
  type Operand (a, b) = (Operand a, Operand b)
  act (a, b) (x, y) = (act a x, act b y)

-- |
-- prop> diff @(Sum Int, Sum Int) x y `act` x == y
instance (Diff a, Diff b) => Diff (a, b) where
  diff (x, y) (x', y') = (diff x x', diff y y')


data PatchEither a b
  = PatchEither a b
  | ReplaceEither (Either (Operand a) (Operand b))
deriving instance (Eq   a, Eq   b, Eq   (Operand a), Eq   (Operand b)) => Eq   (PatchEither a b)
deriving instance (Show a, Show b, Show (Operand a), Show (Operand b)) => Show (PatchEither a b)

instance ( Arbitrary a
         , Arbitrary b
         , Arbitrary (Operand a)
         , Arbitrary (Operand b)
         ) => Arbitrary (PatchEither a b) where
  arbitrary = oneof
            [ PatchEither <$> arbitrary <*> arbitrary
            , ReplaceEither <$> arbitrary
            ]

-- |
-- prop> bimapAction @PatchEither @(Sum Int) @(Sum Int) @(Last Int) @(Last Int) id id id id x == id x
-- prop> (bimapAction @PatchEither @(Sum Int) @(Sum Int) @(Last Int) @(Last Int) f1 f1' f2 f2' . bimapAction @PatchEither @(Sum Int) @(Sum Int) @(Last Int) @(Last Int) g1 g1' g2 g2') x == bimapAction (f1 . g1) (f1' . g1') (f2 . g2) (f2' . g2') x
instance ActionBifunctor PatchEither where
  bimapAction f _  g _  (PatchEither a b) = PatchEither (f a) (g b)
  bimapAction _ f' _ g' (ReplaceEither x) = ReplaceEither (bimap f' g' x)

-- |
-- prop> mempty <> x == (x :: PatchEither (Sum Int) (Last Integer))
-- prop> x <> mempty == (x :: PatchEither (Sum Int) (Last Integer))
-- prop> x <> (y <> z) == (x <> y) <> (z :: PatchEither (Sum Int) (Last Integer))
instance (Action a, Action b) => Monoid (PatchEither a b) where
  mempty = PatchEither mempty mempty
  PatchEither a b `mappend` PatchEither a' b' = PatchEither (a <> a') (b <> b')
  _               `mappend` ReplaceEither x'  = ReplaceEither x'
  ReplaceEither x `mappend` p                 = ReplaceEither $ act p x

-- |
-- prop> act @(PatchEither (Sum Int) (Sum Integer)) mempty   x == id                x
-- prop> act @(PatchEither (Sum Int) (Sum Integer)) (p <> q) x == (act p >>> act q) x
instance (Action a, Action b) => Action (PatchEither a b) where
  type Operand (PatchEither a b) = Either (Operand a) (Operand b)
  act (PatchEither a _) (Left  x) = Left  (act a x)
  act (PatchEither _ b) (Right y) = Right (act b y)
  act (ReplaceEither x) _         = x

-- |
-- prop> diff @(PatchEither (Sum Int) (Sum Integer)) x y `act` x == y
instance (Diff a, Diff b) => Diff (PatchEither a b) where
  diff (Left  x) (Left  x') = PatchEither (diff x x') mempty
  diff (Right y) (Right y') = PatchEither mempty (diff y y')
  diff _         x'         = ReplaceEither x'


-- |
-- prop> act @All mempty   x == id                x
-- prop> act @All (p <> q) x == (act p >>> act q) x
instance Action All where
  type Operand All = Bool
  act (All x) = (&& x)


-- |
-- prop> act @(Alt Maybe Int) mempty   x == id                x
-- prop> act @(Alt Maybe Int) (p <> q) x == (act p >>> act q) x
instance Alternative f => Action (Alt f a) where
  type Operand (Alt f a) = f a
  act (Alt x) = (<|> x)


-- |
-- prop> act @Any mempty   x == id                x
-- prop> act @Any (p <> q) x == (act p >>> act q) x
instance Action Any where
  type Operand Any = Bool
  act (Any x) = (|| x)


-- |
-- prop> act @(Dual (Endo Int)) mempty   x == id                x
-- prop> act @(Dual (Endo Int)) (p <> q) x == (act p >>> act q) x
instance Action (Dual (Endo a)) where
  type Operand (Dual (Endo a)) = a
  act (Dual (Endo f)) = f


-- |
-- prop> act @(Last Int) mempty   x == id                x
-- prop> act @(Last Int) (p <> q) x == (act p >>> act q) x
instance Action (Last a) where
  type Operand (Last a) = a
  act (Last Nothing)   x = x
  act (Last (Just x')) _ = x'

-- |
-- prop> diff @(Last Int) x y `act` x == y
instance Eq a => Diff (Last a) where
  diff x x' | x == x'   = Last $ Nothing
            | otherwise = Last $ Just x'


-- |
-- prop> act @(Product (Ratio Integer)) mempty   x == id                x
-- prop> act @(Product (Ratio Integer)) (p <> q) x == (act p >>> act q) x
instance Num a => Action (Product a) where
  type Operand (Product a) = a
  act (Product x) = (* x)

-- |
-- partial if x' == 0
--
-- prop> y /= 0 ==> diff @(Product (Ratio Integer)) x y `act` x == y
instance Fractional a => Diff (Product a) where
  diff x x' = Product (x' / x)


-- |
-- prop> act @(Sum Int) mempty   x == id                x
-- prop> act @(Sum Int) (p <> q) x == (act p >>> act q) x
instance Num a => Action (Sum a) where
  type Operand (Sum a) = a
  act (Sum x) = (+ x)

-- |
-- prop> diff @(Sum Int) x y `act` x == y
instance Num a => Diff (Sum a) where
  diff x x' = Sum (x' - x)
