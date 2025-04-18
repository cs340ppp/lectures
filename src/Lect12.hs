{-# LANGUAGE InstanceSigs #-}

module Lect12 where

import Prelude hiding (fail)
import Control.Exception
import Data.Char

-- state monad transformer

newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

instance (Monad m) => Functor (StateT s m) where
  fmap :: (a -> b) -> StateT s m a -> StateT s m b
  fmap f (StateT st) = StateT $ \s ->  do
    (x, s') <- st s
    return (f x, s')

instance (Monad m) => Applicative (StateT s m) where
  pure :: a -> StateT s m a
  pure x = StateT $ \s -> return (x, s)

  (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
  StateT mf <*> StateT mx = StateT $ \s -> do
    (f, s') <- mf s
    (x, s'') <- mx s'
    return (f x, s'')

instance (Monad m) => Monad (StateT s m) where
  (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
  StateT st >>= f = StateT $ \s -> do
    (x, s') <- st s
    runStateT (f x) s'

pop :: StateT [a] Maybe a
pop = StateT $ \s -> case s of
                       [] -> Nothing
                       (x:xs) -> Just (x, xs)

push :: a -> StateT [a] Maybe ()
push x = StateT $ \s -> Just ((), x:s)

stackOps :: StateT [Int] Maybe ()
stackOps = push 1 >> pop >> pop >> push 2

-- some simple functions to read/write state

get :: Monad m => StateT s m s
get = StateT $ \s -> return (s, s)

put :: Monad m => s -> StateT s m ()
put s = StateT $ \_ -> return ((), s)

-- lift a value from the base monad into StateT, threading the state through

lift :: Monad m => m a -> StateT s m a
lift m = StateT $ \s -> do x <- m
                           return (x, s)

-- updated guessing game

guess :: StateT (Int,Int) IO ()
guess = do (target,tries) <- get
           lift $ putStrLn "Enter a guess"
           input <- lift getLine
           case reads input of -- reads tries to parse input for Int
             [(guessVal, "")] -> do
               put (target, tries+1)
               if guessVal < target then do
                 lift $ putStrLn "Too low!"
                 guess
               else if guessVal > target then do
                 lift $ putStrLn "Too high!"
                 guess
               else
                 lift $ putStrLn $ "You got it in "++show tries++" tries"
             _ -> do
               lift $ putStrLn "Invalid input."
               guess 
           
-- Parsing with the State monad

type Parser a = StateT String Maybe a

parse = runStateT -- for legibility

char :: Parser Char
char = StateT $ \s -> case s of ""     -> Nothing
                                (c:cs) -> Just (c,cs)

sat :: (Char -> Bool) -> Parser Char
sat p = do c <- char
           if p c
           then return c
           else fail

fail :: Parser a
fail = StateT $ \_ -> Nothing

string :: String -> Parser String
string "" = return ""
string (x:xs) = do sat (== x)
                   string xs
                   return (x:xs)

infixr 2 <|>
(<|>) :: Parser a -> Parser a -> Parser a
p <|> q = StateT $ \s -> case runStateT p s of
                           Nothing -> runStateT q s
                           Just x -> Just x
 

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = do x <- p 
                 xs <- oneOrMore p <|> return []
                 return $ x:xs

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> return []

int :: Parser Int
int = do cs <- oneOrMore (sat isDigit)
         return (read cs)

token :: Parser a -> Parser a
token p = do zeroOrMore $ sat isSpace
             x <- p
             zeroOrMore $ sat isSpace
             return x

symbol :: String -> Parser String
symbol s = token (string s)

-- Expression parser

data Expr = Lit Int | Par Expr | Add Expr Expr | Sub Expr Expr 
            deriving Show

expr :: Parser Expr
expr = do t1 <- term
          op <- sat (== '+') <|> sat (== '-')
          t2 <- term
          return $ (if op == '+' then Add else Sub) t1 t2
       <|>
       term

term :: Parser Expr
term = do symbol "("
          e <- expr
          symbol ")"
          return e
       <|>
       do i <- token int
          return $ Lit i
          

eval :: Expr -> Int
eval (Lit i) = i
eval (Par e) = eval e
eval (Add e1 e2) = eval e1 + eval e2
eval (Sub e1 e2) = eval e1 - eval e2

evalString :: String -> Maybe Int
evalString s = do
  (e, _) <- runStateT expr s
  return $ eval e
