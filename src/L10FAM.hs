{-# LANGUAGE FlexibleInstances, InstanceSigs #-}

module L10FAM where
import Prelude hiding (Functor, fmap, (<$>),
                       Applicative, pure, (<*>),
                       Monad, (>>=), (>>), return)
import Data.List hiding (find)

-- Functors

class Functor f where
  fmap :: (a -> b) -> f a -> f b

instance Functor [] where
  fmap :: (a -> b) -> [a] -> [b]
  fmap = map

instance Functor Maybe where
  fmap :: (a -> b) -> Maybe a -> Maybe b
  fmap _ Nothing = Nothing
  fmap f (Just x) = Just $ f x

infixl 4 <$>
(<$>) :: Functor f => (a -> b) -> f a -> f b
(<$>) = fmap

data Tree a = Node a [Tree a] | Leaf a deriving Show

instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap = undefined

instance Functor ((->) a) where
  fmap :: (b -> c) -> (a -> b) -> (a -> c)
  fmap = undefined

class Functor f => Applicative f where
  pure :: a -> f a
  infixl 4 <*>
  (<*>) :: f (a -> b) -> f a -> f b

find :: (a -> Bool) -> [a] -> Maybe a
find _ [] = Nothing
find p (x:xs) | p x = Just x
              | otherwise = find p xs

-- Applicative Functors

instance Applicative Maybe where
  pure :: a -> Maybe a
  pure = Just
  
  (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
  Nothing  <*> _        = Nothing
  _        <*> Nothing  = Nothing
  (Just f) <*> (Just x) = Just $ f x
  
instance Applicative [] where
  pure :: a -> [a]
  pure x = [x]
  
  (<*>) :: [a -> b] -> [a] -> [b]
  [] <*> _  = []
  _  <*> [] = []
  fs <*> xs = [f x | f <- fs, x <- xs]
  
liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2 f x y = f <$> x <*> y

-- Monads

class Applicative m => Monad m where
  (>>=) :: m a -> (a -> m b) -> m b

  (>>) :: m a -> m b -> m b
  x >> y = x >>= \_ -> y

  return :: a -> m a
  return = pure

instance Monad Maybe where
  (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
  Nothing >>= _ = Nothing
  Just x  >>= f = f x

safeDiv :: Integral a => a -> a -> Maybe a
safeDiv _ 0 = Nothing
safeDiv x y = Just $ x `div` y

fDivs :: Integral a => a -> a -> a -> a -> a -> a -> Maybe a
fDivs a b c d e f = a `safeDiv` b >>= \r1 ->
                    c `safeDiv` d >>= \r2 -> 
                    (r1+r2) `safeDiv` e >>= \r3 ->
                    return (r3 * f)

fDivs' :: Integral a => a -> a -> a -> a -> a -> a -> Maybe a
fDivs' a b c d e f = do r1 <- a `safeDiv` b
                        r2 <- c `safeDiv` d
                        r3 <- (r1+r2) `safeDiv` e
                        return (r3 * f)
