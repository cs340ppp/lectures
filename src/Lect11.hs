{-# LANGUAGE InstanceSigs #-}

module Lect10 where
import Prelude hiding (fail)
import Data.Char
import Control.Exception
import Control.Monad hiding (fail)

-- Logger

data Logger a = Logger { loggerVal  :: a
                       , loggerMsgs :: [String] }
                       deriving (Show)

instance Functor Logger where
  fmap :: (a -> b) -> Logger a -> Logger b
  fmap f (Logger x l) = Logger (f x) l


instance Applicative Logger where
  pure :: a -> Logger a
  pure x = Logger x []

  (<*>) :: Logger (a -> b) -> Logger a -> Logger b
  (Logger f l1) <*> (Logger x l2) = undefined


instance Monad Logger where
  (>>=) :: Logger a -> (a -> Logger b) -> Logger b
  (Logger x l) >>= f = undefined



logVal :: Show a => a -> Logger a
logVal x = Logger x ["Got " ++ show x]

logOp :: Show a => String -> a -> Logger a
logOp op x = Logger x [op ++ " => " ++ show x]

logAppend :: String -> Logger ()
logAppend m = Logger () [m]

log_eg = do logAppend "Start"
            x <- logVal 1
            y <- logOp "Add 20" $ x + 20
            z <- logOp "Double" $ 2 * y
            return z

-- State

data State s a = State { runState :: s -> (a, s) }

pop :: State [a] a
pop = State $ \(x:xs) -> (x, xs)

push :: a -> State [a] ()
push x = undefined

peek :: State [a] a
peek = undefined

instance Functor (State s) where
  fmap :: (a -> b) -> State s a -> State s b
  fmap f st = State $ \s -> let (x, s') = runState st s
                            in (f x, s')

instance Applicative (State s) where
  pure :: a -> State s a
  pure x = State $ \s -> (x, s)

  (<*>) :: State s (a -> b) -> State s a -> State s b
  stf <*> stx = undefined

instance Monad (State s) where
  (>>=) :: State s a -> (a -> State s b) -> State s b
  st >>= f = undefined

stackFoo :: State [Int] ()
stackFoo = do w <- pop
              x <- pop
              let wx = w * x
              y <- pop
              z <- pop
              let yz = y * z
              push $ wx + yz

-- Parser

data Parser a = Parser { parse :: String -> Maybe (a, String) }

instance Functor Parser where
  fmap f p = Parser $ \s -> case parse p s
                              of Nothing     -> Nothing
                                 Just (x,s') -> Just (f x,s')

instance Applicative Parser where
  pure x = Parser $ \s -> Just (x,s)

  pf <*> px = Parser $ \s -> case parse pf s
                               of Nothing     -> Nothing
                                  Just (f,s') -> parse (f <$> px) s'

instance Monad Parser where
  px >>= f = Parser $ \s -> case parse px s
                              of Nothing     -> Nothing
                                 Just (x,s') -> parse (f x) s'

char :: Parser Char
char = Parser $ \s -> case s of "" -> Nothing
                                (c:cs) -> Just (c,cs)

sat :: (Char -> Bool) -> Parser Char
sat p = do c <- char
           if p c then return c else fail

fail :: Parser a
fail = Parser $ \s -> Nothing


string :: String -> Parser String
string "" = return ""
string (x:xs) = do sat (== x)
                   string xs
                   return (x:xs)

infixr 2 <|>
(<|>) :: Parser a -> Parser a -> Parser a
p <|> q = Parser $ \s -> case parse p s
                           of Nothing -> parse q s
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

-- IO

main :: IO ()
main = do name <- getLine
          putStrLn $ "Hello, " ++ name


guess :: Int -> IO ()
guess n = do putStr "Enter a guess: "
             g <- readLn
             case compare g n
               of LT -> putStrLn "Too small!" >> guess n
                  GT -> putStrLn "Too big!"   >> guess n
                  _  -> putStrLn "Good guess!"

guess' :: Int -> IO ()
guess' n = do putStr "Enter a guess: "
              g <- catch readLn handler
              case compare g n
                of LT -> putStrLn "Too small!" >> guess' n
                   GT -> putStrLn "Too big!"   >> guess' n
                   _  -> putStrLn "Good guess!"
 where handler e = do putStrLn $ show (e :: IOError)
                      return 0

-- Monad utilities

getLines :: IO [String]
getLines = sequence [getLine, getLine, getLine]

getNumbers :: IO [Int]
getNumbers = replicateM 3 $ do
                putStrLn "Enter a number: "
                readLn
  
greetAll :: [String] -> IO ()
greetAll names = forM_ names $ \name -> do
                    putStrLn $ "Hello, " ++ name
                    putStrLn "How are you?"

