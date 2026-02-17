{-# LANGUAGE InstanceSigs #-}

module L12Trans where

import Prelude hiding (fail)
import Control.Exception
import Data.Char
import System.Random

-- state monad transformer

newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

instance (Monad m) => Functor (StateT s m) where
  fmap :: (a -> b) -> StateT s m a -> StateT s m b
  fmap f (StateT st) = StateT $ \s ->  do
    (x, s') <- st s
    return (f x, s')

instance (Monad m) => Applicative (StateT s m) where
  pure :: a -> StateT s m a
  pure x = undefined

  (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
  StateT mf <*> StateT mx = undefined

instance (Monad m) => Monad (StateT s m) where
  (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
  StateT st >>= f = undefined

pop :: StateT [a] Maybe a
pop = StateT $ \s -> case s of
                       [] -> Nothing
                       (x:xs) -> Just (x, xs)

push :: a -> StateT [a] Maybe ()
push x = StateT $ \s -> Just ((), x:s)

stackOps :: StateT [Int] Maybe ()
stackOps = push 1 >> pop >> pop >> push 2

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
term = undefined
          
eval :: Expr -> Int
eval (Lit i) = i
eval (Par e) = eval e
eval (Add e1 e2) = eval e1 + eval e2
eval (Sub e1 e2) = eval e1 - eval e2

evalString :: String -> Maybe Int
evalString s = undefined

-- lift a value from the base monad into StateT, threading the state through

lift :: Monad m => m a -> StateT s m a
lift m = StateT $ \s -> do x <- m
                           return (x, s)

-- get/put for StateT

get :: Monad m => StateT s m s
get = StateT $ \s -> return (s, s)

put :: Monad m => s -> StateT s m ()
put s = StateT $ \_ -> return ((), s)

-- updated guessing game that tracks all guesses

guess :: Int -> StateT [Int] IO ()
guess n = do lift $ putStrLn "Enter a guess"
             input <- lift getLine
             case reads input of
               [(g, "")] -> do
                 guesses <- get
                 put (g:guesses)
                 if g < n then do
                   lift $ putStrLn "Too small!"
                   guess n
                 else if g > n then do
                   lift $ putStrLn "Too big!"
                   guess n
                 else do
                   lift $ putStrLn "Good guess!"
               _ -> do
                 lift $ putStrLn "Invalid input."
                 guess n

main :: IO ()
main = do g <- getStdGen
          let (n,_) = randomR (0,100) g
          (_,gs) <- runStateT (guess n) []
          putStrLn $ "Guesses " ++ show gs
          return ()
           
-- Identity monad

newtype Identity a = Identity a deriving Show

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance Applicative Identity where
  pure x = Identity x
  Identity f <*> Identity x = Identity $ f x

instance Monad Identity where
  Identity x >>= f = f x

-- "plain" State monad

type State s a = StateT s Identity a

-- convenience constructor

state :: Monad m => (s -> (a,s)) -> StateT s m a
state f = StateT (return . f)
