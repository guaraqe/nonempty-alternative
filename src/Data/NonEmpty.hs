{-| This package extends @NonEmpty@ from @semigroups@ to arbitrary
@Alternative@ types. The method is the same as for lists, by
separating an element from the rest.

There are two natural ways to merge an element @x@ to the rest of the
structure @xs@. The first gives rise to @NonEmptyL@:

> flattenL :: NonEmptyL f a -> f a
> flattenL (x :<: xs) = pure x <|> xs

The second gives rise to @NonEmptyR@:

> flattenR :: NonEmptyR f a -> f a
> flattenR (xs :>: x) = xs <|> pure x

The instances are made so that @flattenL@ gives a type class morphism
between @NonEmptyL List@ and @List@, and @flattenR@ gives the same for
@NonEmptyR RList@ and @RList@ from the package @rlist@.
-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.NonEmpty
       (
       -- * Left Non-Empty Alternatives
         NonEmptyL (..)
       -- * Basic functions for `NonEmptyL`
       , headL
       , tailL
       , flattenL
       , joinL
       , budgeL
       -- * Right Non-Empty Alternatives
       , NonEmptyR (..)
       -- * Basic functions for `NonEmptyR`
       , lastR
       , initR
       , flattenR
       , joinR
       , budgeR
       ) where

import Prelude hiding (head, tail)
import Data.Data
import GHC.Generics

import Data.Foldable
import Data.Semigroup
import Control.Applicative
import Control.Comonad

----------------------------------------------------------------------

-- | The type @NonEmptyL@ is well suited for `cons` structures.
data NonEmptyL f a = a :<: f a
  deriving (Show, Eq, Ord,Read, Data, Typeable, Generic, Generic1)

infixr 5 :<:

-- | The type @NonEmptyR@ is well suited for `snoc` structures.
data NonEmptyR f a = f a :>: a
  deriving (Show, Eq, Ord,Read, Data, Typeable, Generic, Generic1)

infixl 5 :>:

----------------------------------------------------------------------

instance Functor f => Functor (NonEmptyL f) where
  fmap f (x :<: xs) = (f x) :<: (f <$> xs)

instance Functor f => Functor (NonEmptyR f) where
  fmap f (xs :>: x) = (f <$> xs) :>: (f x)

----------------------------------------------------------------------

instance Alternative f => Applicative (NonEmptyL f) where
  pure x = x :<: empty

  (f :<: fs) <*> (x :<: xs) = (f x) :<: (   (pure f <*> xs    )
                                     <|> (fs     <*> (pure x <|> xs)))

instance Alternative f => Applicative (NonEmptyR f) where
  pure x = empty :>: x

  (fs :>: f) <*> (xs :>: x) = (   (fs     <*> (xs <|> pure x) )
                            <|> (pure f <*> xs    ) ) :>: (f x)

----------------------------------------------------------------------

instance (Alternative f, Monad f) => Monad (NonEmptyL f) where
  (x :<: xs) >>= f = y :<: (ys <|> zs)
                  where (y :<: ys) = f x
                        zs = xs >>= flattenL . f

----------------------------------------------------------------------

instance Alternative f => Comonad (NonEmptyL f) where
  extract = headL
  duplicate (x :<: xs) = (x :<: xs) :<: (fmap (:<: empty) xs)

instance Alternative f => Comonad (NonEmptyR f) where
  extract = lastR
  duplicate (xs :>: x) = (fmap (empty :>:) xs) :>: (xs :>: x)

----------------------------------------------------------------------

instance Foldable f => Foldable (NonEmptyL f) where
  foldr f z (x :<: xs) = f x (foldr f z xs)
  foldr' f z (x :<: xs) = f x (foldr' f z xs)
  foldr1 f (x :<: xs) = if null xs
                          then x
                          else f x (foldr1 f xs)
  foldl f z (x :<: xs) = foldl f (f z x) xs
  foldl' f z (x :<: xs) = foldl' f (f z x) xs
  foldl1 f (x :<: xs) = foldl f x xs

instance Foldable f => Foldable (NonEmptyR f) where
  foldr f z (xs :>: x) = foldr f (f x z) xs
  foldr' f z (xs :>: x) = foldr' f (f x z) xs
  foldr1 f (xs :>: x) = foldr f x xs
  foldl f z (xs :>: x) = f (foldl f z xs) x
  foldl' f z (xs :>: x) = f (foldl' f z xs) x
  foldl1 f (xs :>: x) = if null xs
                          then x
                          else f (foldl1 f xs) x

----------------------------------------------------------------------

instance (Functor f, Traversable f) => Traversable (NonEmptyL f) where
  traverse f (x :<: xs) = (:<:) <$> f x
                              <*> traverse f xs

instance (Functor f, Traversable f) => Traversable (NonEmptyR f) where
  traverse f (xs :>: x) = (:>:) <$> traverse f xs
                              <*> f x

----------------------------------------------------------------------

instance Alternative f => Semigroup (NonEmptyL f a) where
  (x :<: xs) <> (y :<: ys) = x :<: (xs <|> pure y <|> ys)

instance Alternative f => Semigroup (NonEmptyR f a) where
  (xs :>: x) <> (ys :>: y) = (xs <|> pure x <|> ys) :>: y

----------------------------------------------------------------------

-- | Extracts the structure's singular element. This function is total
-- and equivalent to @extract@ from @Comonad@.
headL :: NonEmptyL f a -> a
headL (x :<: _) = x

-- | Extracts the structure's remaining data. This function is total.
tailL :: NonEmptyL f a -> f a
tailL (_ :<: xs) = xs

-- | Flattens the structure to its base type from the left.
flattenL :: Alternative f => NonEmptyL f a -> f a
flattenL (x :<: xs) = pure x <|> xs

-- | This is equivalent to @join@ for @Monad@.
joinL :: (Alternative f, Monad f)
      => NonEmptyL f (NonEmptyL f a) -> NonEmptyL f a
joinL ((x :<: xs) :<: ys) = x :<: (xs <|> (ys >>= flattenL))

-- | Budge the head into the remaining structure from the left, adding
-- an empty head.
budgeL :: (Alternative f, Alternative g)
       => NonEmptyL f (g a) -> NonEmptyL f (g a)
budgeL = (empty :<:) . flattenL

----------------------------------------------------------------------

-- | Extracts the structure's singular element. This function is total
-- and equivalent to @extract@ from @Comonad@.
lastR :: NonEmptyR f a -> a
lastR (_ :>: x) = x

-- | Extracts the structure's remaining data. This function is total.
initR :: NonEmptyR f a -> f a
initR (xs :>: _) = xs

-- | Flattens the structure to its base type from the right.
flattenR :: Alternative f => NonEmptyR f a -> f a
flattenR (xs :>: x) = xs <|> pure x

-- | This is equivalent to @join@ for @Monad@.
joinR :: (Alternative f, Monad f)
      => NonEmptyR f (NonEmptyR f a) -> NonEmptyR f a
joinR (ys :>: (xs :>: x)) = ((ys >>= flattenR) <|> xs) :>: x

-- | Budge the head into the remaining structure from the right,
-- adding an empty head.
budgeR :: (Alternative f, Alternative g)
       => NonEmptyR f (g a) -> NonEmptyR f (g a)
budgeR = (:>: empty) . flattenR
