{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFunctor #-}

module NaiveFree where

import Control.Monad
import Control.Monad.Free(MonadFree(..))
import Prelude.Extras

{-
   The free monad generated by composing a functor with itself 0-n times.
   >>= is horrendously inefficient; we must bubble from the top all the way to the
   leaves of the tree every time!
-}
data NaiveFree f a
  = Wrap (f (NaiveFree f a))
  | Pure a
  deriving Functor


-- Keep binding down layer by layer, until we reach the leaf nodes; then we can
-- substitute them with a new layer.
naiveBind :: (Functor f) => NaiveFree f a -> (a -> NaiveFree f b) -> NaiveFree f b
naiveBind (Wrap ffa) f = Wrap $ fmap (\ x -> naiveBind x f) ffa
naiveBind (Pure a) f = f a



naiveFoldFree :: Monad m => (forall x . f x -> m x) -> NaiveFree f a -> m a
naiveFoldFree f (Pure a) = pure a
naiveFoldFree f (Wrap ffa) = f ffa >>= naiveFoldFree f

-- Instances
instance Functor f => Applicative (NaiveFree f) where
  (<*>) = ap
  pure = Pure

instance Functor f => Monad (NaiveFree f) where
  (>>=) = naiveBind

instance (Functor f) => MonadFree f (NaiveFree f) where
  wrap = Wrap

instance (Eq a, Eq (f (NaiveFree f a))) => Eq (NaiveFree f a) where
  (Pure a) == (Pure b) = a == b
  (Wrap x) == (Wrap y) = x == y

instance (Show a, Show1 f) => Show (NaiveFree f a) where
  showsPrec p (Pure   a) = showParen (p > 0) $ ("Pure "   ++) . showsPrec  11 a
  showsPrec p (Wrap a) = showParen (p > 0) $ ("Wrap " ++) . showsPrec1 11 a
