-- CS 340: Programming Paradigms and Patterns
-- Lect 11 - Monadic Parsing
-- Michael Lee

module Lect11 where
import Prelude hiding (fail)
import Data.Char

data State s a = State { runState :: s -> (s, a) }


instance Functor (State s) where
  fmap :: (a -> b) -> State s a -> State s b
  fmap f st = State $ \s -> let (s', x) = runState st s
                            in (s', f x)


instance Applicative (State s) where
  pure :: a -> State s a
  pure x = State $ \s -> (s, x)

  (<*>) :: State s (a -> b) -> State s a -> State s b
  stf <*> stx = State $ \s -> let (s', f) = runState stf s
                              in runState (f <$> stx) s'


instance Monad (State s) where
  (>>=) :: State s a -> (a -> State s b) -> State s b
  st >>= f = State $ \s -> let (s', x) = runState st s
                           in runState (f x) s'


-- Problem: using the state monad, write a parser that attempts to parse and
--          evaluate an infix arithmetic expression

-- Example: "1 + 2 * 3" -> 7
--          "1 + 2 * 3 + 4" -> 11
--          "(1 + 2) * (3 + 4))" -> 21



-- Grammar (in Backus-Naur Form) for infix arithmetic expressions:
--
--   expr   ::= term + expression | term
--   term   ::= factor * term | factor
--   factor ::= ( expr ) | integer
