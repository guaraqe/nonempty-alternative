{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.NonEmpty
       (
       -- * The type of non-empty alternatives
         NonEmpty (..)
       -- * Basic functions
       , head
       , tail
       , flattenLeft
       , flattenRight ) where

import Prelude hiding (head, tail)
import Data.Data
import GHC.Generics

import Data.Foldable
import Data.Semigroup
import Control.Applicative

----------------------------------------------------------------------

-- | NonEmpty is naturally extended from `List` to any `Alternative`
-- type. See the source for the instance definitions.
data NonEmpty f a = a :| f a
  deriving (Show, Eq, Ord,Read, Data, Typeable, Generic, Generic1)

infixr 5 :|

instance Functor f => Functor (NonEmpty f) where

  fmap f (x :| xs) = (f x) :| (fmap f xs)

instance Alternative f => Applicative (NonEmpty f) where

  pure x = x :| empty

  (f :| fs) <*> (x :| xs) = (f x) :| (fs <*> xs)

-- | Flattens a `NonEmpty` from the left.
flattenLeft :: Alternative f => NonEmpty f a -> f a
flattenLeft (x :| xs) = pure x <|> xs

-- | Flattens a `NonEmpty` from the right.
flattenLeft :: Alternative f => NonEmpty f a -> f a
flattenRight :: Alternative f => NonEmpty f a -> f a
flattenRight (x :| xs) = xs <|> pure x

instance (Alternative f, Monad f) => Monad (NonEmpty f) where

  (x :| xs) >>= f = y :| (ys <|> zs)
     where y :| ys = f x
           zs = xs >>= flattenLeft . f

instance Foldable f => Foldable (NonEmpty f) where

  foldr f z (x :| xs) = f x (foldr f z xs)
  foldr' f z (x :| xs) = f x (foldr' f z xs)
  foldr1 f (x :| xs) = foldr f x xs
  foldl f z (x :| xs) = foldl f (f z x) xs
  foldl' f z (x :| xs) = foldl' f (f z x) xs
  foldl1 f (x :| xs) = foldl f x xs

instance (Alternative f, Traversable f) => Traversable (NonEmpty f) where

  traverse f (x :| xs) = (:|) <$> f x
                              <*> traverse f xs

instance Alternative f => Semigroup (NonEmpty f a) where

  (x :| xs) <> (y :| ys) = x :| (xs <|> pure y <|> ys)

-- | Head is a total function for `NonEmpty`.
head :: NonEmpty f a -> a
head (x :| _) = x

-- | Tail is a total function for `NonEmpty`.
tail :: NonEmpty f a -> f a
tail (_ :| xs) = xs
