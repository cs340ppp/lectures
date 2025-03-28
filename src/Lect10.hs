{-# LANGUAGE FlexibleInstances, InstanceSigs #-}

module Lect10 where
import Prelude hiding (Functor, fmap, (<$>),
                       Applicative, pure, (<*>),
                       Monad, (>>=), (>>), return)
import Data.List hiding (find)

class Functor f where
  fmap :: (a -> b) -> f a -> f b

instance Functor [] where
  fmap :: (a -> b) -> [a] -> [b]
  fmap = map

instance Functor Maybe where
  fmap :: (a -> b) -> Maybe a -> Maybe b
  fmap = undefined

(<$>) :: Functor f => (a -> b) -> f a -> f b
(<$>) = fmap

class Functor f => Applicative f where
  pure :: a -> f a
  
  (<*>) :: f (a -> b) -> f a -> f b

instance Applicative Maybe where
  pure :: a -> Maybe a
  pure = undefined
  
  (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
  (<*>) = undefined

class Applicative m => Monad m where
  (>>=) :: m a -> (a -> m b) -> m b

  (>>) :: m a -> m b -> m b
  x >> y = x >>= \_ -> y

  return :: a -> m a
  return = pure
