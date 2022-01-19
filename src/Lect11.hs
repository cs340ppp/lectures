-- CS 340: Programming Paradigms and Patterns
-- Lect 11 - Monadic Parsing
-- Michael Lee

module Lect11 where
import Data.Char

data State s a = State { run :: s -> (a, s) }


instance Functor (State s) where
  fmap f st = State $ \s -> let (x, s') = run st s
                            in (f x, s')



instance Applicative (State s) where
  pure x = State $ \s -> (x, s)
  stf <*> stx = State $ \s -> let (f, s') = run stf s
                              in run (f <$> stx) s'


instance Monad (State s) where
  st >>= f = State $ \s -> let (x, s') = run st s
                           in run (f x) s'
