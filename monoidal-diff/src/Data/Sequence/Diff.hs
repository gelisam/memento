{-# LANGUAGE FlexibleContexts, StandaloneDeriving, TypeFamilies, UndecidableInstances, ViewPatterns #-}
module Data.Sequence.Diff
  ( PatchL(..), PatchR(..)
  ) where

import Data.Monoid
import Data.Sequence (Seq)
import Test.QuickCheck
import qualified Data.Sequence as Seq

import MonoidalDiff

-- $setup
-- >>> :set -XFlexibleInstances
-- >>> :set -XTypeApplications
-- >>> import Control.Arrow ((>>>))
-- >>> import Data.Sequence ((|>), (<|))
--
-- QuickCheck requires a 'Show' instance, but it won't be used
-- since the test passes.
--
-- >>> instance Show (     Int ->      Int) where show f = "fmap f [0..10] = " <> show (fmap f [0..10])
-- >>> instance Show (Sum  Int -> Sum  Int) where show f = "fmap (f . Sum) [0..10] = " <> show (fmap (f . Sum) [0..10])

-- | A version of Seq.zip which keeps the leftovers.
--
-- >>> zipL (Seq.fromList "abc") (Seq.fromList [1,2,3,4::Int])
-- (fromList [('a',1),('b',2),('c',3)],fromList "",fromList [4])
zipL :: Seq a -> Seq b -> (Seq (a,b), Seq a, Seq b)
zipL = zipLWith (,)

zipLWith :: (a -> b -> c) -> Seq a -> Seq b -> (Seq c, Seq a, Seq b)
zipLWith f xs ys = (zs, xs', ys')
  where
    zs = Seq.zipWith f xs ys
    n = length zs
    xs' = Seq.drop n xs
    ys' = Seq.drop n ys

-- |
-- >>> zipR (Seq.fromList "abc") (Seq.fromList [0,1,2,3::Int])
-- (fromList "",fromList [0],fromList [('a',1),('b',2),('c',3)])
zipR :: Seq a -> Seq b -> (Seq a, Seq b, Seq (a,b))
zipR = zipRWith (,)

zipRWith :: (a -> b -> c) -> Seq a -> Seq b -> (Seq a, Seq b, Seq c)
zipRWith f xs ys = (xs', ys', zs)
  where
    n = min (length xs) (length ys)
    (xs',xs'') = Seq.splitAt (length xs - n) xs
    (ys',ys'') = Seq.splitAt (length ys - n) ys
    zs = Seq.zipWith f xs'' ys''


-- | Deletes then adds elements at the _left_ end of the sequence.
data PatchL a = PatchL
  { patchLElementsToDelete :: Seq ()  -- makes the code clearer than with Int
  , patchLElementsToModify :: Seq a
  , patchLElementsToAdd    :: Seq (Operand a)
  }
deriving instance (Eq   a, Eq   (Operand a)) => Eq   (PatchL a)
deriving instance (Show a, Show (Operand a)) => Show (PatchL a)

instance (Arbitrary a, Arbitrary (Operand a)) => Arbitrary (PatchL a) where
  arbitrary = PatchL <$> arbitrary <*> arbitrary <*> arbitrary
  shrink (PatchL xs1 xs2 xs3) = (PatchL <$> shrink xs1 <*> pure xs2 <*> pure xs3)
                             <> (PatchL <$> pure xs1 <*> shrink xs2 <*> pure xs3)
                             <> (PatchL <$> pure xs1 <*> pure xs2 <*> shrink xs3)

-- |
-- prop> mapAction @PatchL @(Sum Int) @(Sum Int) id id x == id x
-- prop> (mapAction @PatchL @(Sum Int) @(Sum Int) f f' . mapAction @PatchL @(Sum Int) @(Sum Int) g g') x == mapAction (f . g) (f' . g') x
instance ActionFunctor PatchL where
  mapAction f f' (PatchL delete modify add) = PatchL delete (fmap f modify) (fmap f' add)

-- |
-- It's a bit complicated, so let's look at a few examples patches:
--
-- a  b  c  d  e
-- -------------
-- X  X  X |.  .  delete0
--         |f  f  modify0
--    b' c'|.  .  add0
-- -------------
--    X |.  .  .  delete1
--      |f  f  f  modify1
-- a" b"|.  .  .  add1
-- -------------
-- X  X  X  X |.  delete2
--            |f  modify2
--        d'''|.  add2
--
-- >>> let modify expected replacement = Dual $ Endo $ \x -> if x == expected then replacement else "expected (" <> expected <>"), got (" <> x <> ")"
-- >>> let abcde = "a" <| "b" <| "c" <| "d" <| "e" <| mempty
-- >>> let patch0 = PatchL (Seq.replicate 3 ()) (modify "d" "d'" <| modify "e" "e'" <| mempty) ("b'" <| "c'" <| mempty)
-- >>> let patch1 = PatchL (Seq.replicate 1 ()) (modify "c'" "c''" <| modify "d'" "d''" <| modify "e'" "e''" <| mempty) ("a''" <| "b''" <| mempty)
-- >>> let patch2 = PatchL (Seq.replicate 4 ()) (modify "e''" "e'''" <| mempty) ("d'''" <| mempty)
-- >>> act patch0 abcde
-- fromList ["b'","c'","d'","e'"]
-- >>> act (patch0 <> patch1) abcde
-- fromList ["a''","b''","c''","d''","e''"]
-- >>> act (patch0 <> patch1 <> patch2) abcde
-- fromList ["d'''","e'''"]
-- 
-- prop> mempty <> x == (x :: PatchL (Sum Int))
-- prop> x <> mempty == (x :: PatchL (Sum Int))
-- prop> x <> (y <> z) == (x <> y) <> (z :: PatchL (Sum Int))
instance Action a => Monoid (PatchL a) where
  mempty = PatchL mempty mempty mempty
  PatchL delete1 modify1 add1 `mappend` PatchL delete2 modify2 add2
    = PatchL delete' modify' add'
    where
      -- delete2 first deletes the nodes which were added by add1
      (_, delete2', add1') = zipL delete2 add1

      -- then the delete2' leftovers delete nodes from modify1
      (_, _, modify1') = zipL delete2' modify1

      -- modify2 applies to the remaining add1' and modify1'
      (add1''a, modify2', add1''b) = zipLWith act modify2 add1'
      (modify'a, modify'b, modify'c) = zipLWith mappend modify1' modify2'

      delete' = delete1 <> delete2'
      modify' = modify'a <> modify'b <> modify'c
      add'    = add2 <> add1''a <> add1''b

-- |
-- prop> act @(PatchL (Sum Int)) mempty   x == id                x
-- prop> act @(PatchL (Sum Int)) (p <> q) x == (act p >>> act q) x
instance Action a => Action (PatchL a) where
  type Operand (PatchL a) = Seq (Operand a)
  act (PatchL delete modify add) xs = xs''
    where
      (_, _, xs') = zipL delete xs
      (xs''a, _, xs''b) = zipLWith act modify xs'
      xs'' = add <> xs''a <> xs''b


-- | Deletes then adds elements at the _right_ end of the sequence.
data PatchR a = PatchR
  { patchRElementsToDelete :: Seq ()
  , patchRElementsToModify :: Seq a
  , patchRElementsToAdd    :: Seq (Operand a)
  }
deriving instance (Eq   a, Eq   (Operand a)) => Eq   (PatchR a)
deriving instance (Show a, Show (Operand a)) => Show (PatchR a)

instance (Arbitrary a, Arbitrary (Operand a)) => Arbitrary (PatchR a) where
  arbitrary = PatchR <$> arbitrary <*> arbitrary <*> arbitrary
  shrink (PatchR xs1 xs2 xs3) = (PatchR <$> shrink xs1 <*> pure xs2 <*> pure xs3)
                             <> (PatchR <$> pure xs1 <*> shrink xs2 <*> pure xs3)
                             <> (PatchR <$> pure xs1 <*> pure xs2 <*> shrink xs3)

-- |
-- prop> mapAction @PatchR @(Sum Int) @(Sum Int) id id x == id x
-- prop> (mapAction @PatchR @(Sum Int) @(Sum Int) f f' . mapAction @PatchR @(Sum Int) @(Sum Int) g g') x == mapAction (f . g) (f' . g') x
instance ActionFunctor PatchR where
  mapAction f f' (PatchR delete modify add) = PatchR delete (fmap f modify) (fmap f' add)

-- |
-- a  b  c  d  e
-- -------------
-- .  .| X  X  X  delete0
-- f  f|          modify0
-- .  .| c' d'    add0
-- -------------
-- .  .  .| X     delete1
-- f  f  f|       modify1
-- .  .  .| d" e" add1
-- -------------
-- .| X  X  X  X  delete2
-- f|             modify2
-- .| b'''        add2
--
-- >>> let modify expected replacement = Dual $ Endo $ \x -> if x == expected then replacement else "expected (" <> expected <>"), got (" <> x <> ")"
-- >>> let abcde = "a" <| "b" <| "c" <| "d" <| "e" <| mempty
-- >>> let patch0 = PatchR (Seq.replicate 3 ()) (modify "a" "a'" <| modify "b" "b'" <| mempty) ("c'" <| "d'" <| mempty)
-- >>> let patch1 = PatchR (Seq.replicate 1 ()) (modify "a'" "a''" <| modify "b'" "b''" <| modify "c'" "c''" <| mempty) ("d''" <| "e''" <| mempty)
-- >>> let patch2 = PatchR (Seq.replicate 4 ()) (modify "a''" "a'''" <| mempty) ("b'''" <| mempty)
-- >>> act patch0 abcde
-- fromList ["a'","b'","c'","d'"]
-- >>> act (patch0 <> patch1) abcde
-- fromList ["a''","b''","c''","d''","e''"]
-- >>> act (patch0 <> patch1 <> patch2) abcde
-- fromList ["a'''","b'''"]
-- 
-- prop> mempty <> x == (x :: PatchR (Sum Int))
-- prop> x <> mempty == (x :: PatchR (Sum Int))
-- prop> x <> (y <> z) == (x <> y) <> (z :: PatchR (Sum Int))
instance Action a => Monoid (PatchR a) where
  mempty = PatchR mempty mempty mempty
  PatchR delete1 modify1 add1 `mappend` PatchR delete2 modify2 add2
    = PatchR delete' modify' add'
    where
      -- delete2 first deletes the nodes which were added by add1
      (delete2', add1', _) = zipR delete2 add1

      -- then the delete2' leftovers delete nodes from modify1
      (_, modify1', _) = zipR delete2' modify1

      -- modify2 applies to the remaining add1' and modify1'
      (modify2', add1''a, add1''b) = zipRWith act modify2 add1'
      (modify'a, modify'b, modify'c) = zipRWith mappend modify1' modify2'

      delete' = delete1 <> delete2'
      modify' = modify'a <> modify'b <> modify'c
      add'    = add1''a <> add1''b <> add2

-- |
-- prop> act @(PatchR (Sum Int)) mempty   x == id                x
-- prop> act @(PatchR (Sum Int)) (p <> q) x == (act p >>> act q) x
instance Action a => Action (PatchR a) where
  type Operand (PatchR a) = Seq (Operand a)
  act (PatchR delete modify add) xs = xs''
    where
      (xs', _, _) = zipR xs delete
      (_, xs''a, xs''b) = zipRWith act modify xs'
      xs'' = xs''a <> xs''b <> add
