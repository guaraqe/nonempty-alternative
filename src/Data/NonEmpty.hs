{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.NonEmpty
       (
       -- * The type of left non-empty alternatives
         NonEmptyL (..)
       -- * Basic functions for `NonEmptyL`
       , headL
       , tailL
       , flattenL
       , joinL
       -- * The type of right non-empty alternatives
       , NonEmptyR (..)
       -- * Basic functions for `NonEmptyR`
       , lastR
       , initR
       , flattenR
       , joinR
       ) where

import Prelude hiding (head, tail)
import Data.Data
import GHC.Generics

import Data.Foldable
import Data.Semigroup
import Control.Applicative
import Control.Comonad

----------------------------------------------------------------------

-- | NonEmptyL is naturally extended from `List` to any `Alternative`
-- type in two different ways. They are differentiated by their
-- instances.
-- The `L`eft one is well suited for `cons` structures.
data NonEmptyL f a = a :< f a
  deriving (Show, Eq, Ord,Read, Data, Typeable, Generic, Generic1)

infixr 5 :<

-- | The `R`ight one is well suited for `snoc` structures.
data NonEmptyR f a = f a :> a
  deriving (Show, Eq, Ord,Read, Data, Typeable, Generic, Generic1)

infixl 5 :>

----------------------------------------------------------------------

instance Functor f => Functor (NonEmptyL f) where
  fmap f (x :< xs) = (f x) :< (f <$> xs)

instance Functor f => Functor (NonEmptyR f) where
  fmap f (xs :> x) = (f <$> xs) :> (f x)

----------------------------------------------------------------------

instance Alternative f => Applicative (NonEmptyL f) where
  pure x = x :< empty

  (f :< fs) <*> (x :< xs) = (f x) :< (   (pure f <*> xs    )
                                     <|> (fs     <*> (pure x <|> xs)))

instance Alternative f => Applicative (NonEmptyR f) where
  pure x = empty :> x

  (fs :> f) <*> (xs :> x) = (   (fs     <*> (xs <|> pure x) )
                            <|> (pure f <*> xs    ) ) :> (f x)

----------------------------------------------------------------------

instance (Alternative f, Monad f) => Monad (NonEmptyL f) where
  (x :< xs) >>= f = y :< (ys <|> zs)
                  where (y :< ys) = f x
                        zs = xs >>= flattenL . f

----------------------------------------------------------------------

instance Alternative f => Comonad (NonEmptyL f) where
  extract = headL
  duplicate (x :< xs) = (x :< xs) :< (fmap (:< empty) xs)

instance Alternative f => Comonad (NonEmptyR f) where
  extract = lastR
  duplicate (xs :> x) = (fmap (empty :>) xs) :> (xs :> x)

----------------------------------------------------------------------

instance Foldable f => Foldable (NonEmptyL f) where
  foldr f z (x :< xs) = f x (foldr f z xs)
  foldr' f z (x :< xs) = f x (foldr' f z xs)
  foldr1 f (x :< xs) = if null xs
                          then x
                          else f x (foldr1 f xs)
  foldl f z (x :< xs) = foldl f (f z x) xs
  foldl' f z (x :< xs) = foldl' f (f z x) xs
  foldl1 f (x :< xs) = foldl f x xs

instance Foldable f => Foldable (NonEmptyR f) where
  foldr f z (xs :> x) = foldr f (f x z) xs
  foldr' f z (xs :> x) = foldr' f (f x z) xs
  foldr1 f (xs :> x) = foldr f x xs
  foldl f z (xs :> x) = f (foldl f z xs) x
  foldl' f z (xs :> x) = f (foldl' f z xs) x
  foldl1 f (xs :> x) = if null xs
                          then x
                          else f (foldl1 f xs) x

----------------------------------------------------------------------

instance (Functor f, Traversable f) => Traversable (NonEmptyL f) where
  traverse f (x :< xs) = (:<) <$> f x
                              <*> traverse f xs

instance (Functor f, Traversable f) => Traversable (NonEmptyR f) where
  traverse f (xs :> x) = (:>) <$> traverse f xs
                              <*> f x

----------------------------------------------------------------------

instance Alternative f => Semigroup (NonEmptyL f a) where
  (x :< xs) <> (y :< ys) = x :< (xs <|> pure y <|> ys)

instance Alternative f => Semigroup (NonEmptyR f a) where
  (xs :> x) <> (ys :> y) = (xs <|> pure x <|> ys) :> y

----------------------------------------------------------------------

headL :: NonEmptyL f a -> a
headL (x :< _) = x

tailL :: NonEmptyL f a -> f a
tailL (_ :< xs) = xs

flattenL :: Alternative f => NonEmptyL f a -> f a
flattenL (x :< xs) = pure x <|> xs

joinL :: (Alternative f, Monad f)
      => NonEmptyL f (NonEmptyL f a) -> NonEmptyL f a
joinL ((x :< xs) :< ys) = x :< (xs <|> (ys >>= flattenL))


lastR :: NonEmptyR f a -> a
lastR (_ :> x) = x

initR :: NonEmptyR f a -> f a
initR (xs :> _) = xs

flattenR :: Alternative f => NonEmptyR f a -> f a
flattenR (xs :> x) = xs <|> pure x

joinR :: (Alternative f, Monad f)
      => NonEmptyR f (NonEmptyR f a) -> NonEmptyR f a
joinR (ys :> (xs :> x)) = ((ys >>= flattenR) <|> xs) :> x
