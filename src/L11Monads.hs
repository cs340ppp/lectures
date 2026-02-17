{-# LANGUAGE InstanceSigs #-}

module L11Monads where
import Prelude hiding (fail)
import Data.Char
import Control.Applicative
import Control.Exception
import Control.Monad hiding (fail)
import System.Random

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
  Logger f l1 <*> Logger x l2 = undefined

instance Monad Logger where
  (>>=) :: Logger a -> (a -> Logger b) -> Logger b
  Logger x l >>= f = undefined

logVal :: Show a => a -> Logger a
logVal x = Logger x ["Got " ++ show x]

logOp :: Show a => String -> a -> Logger a
logOp op x = Logger x [op ++ " => " ++ show x]

log_eg = do x <- logVal 1
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
  fmap f (State st) = State $ \s -> let (x, s') = st s
                                    in (f x, s')

instance Applicative (State s) where
  pure :: a -> State s a
  pure x = State $ \s -> (x, s)

  (<*>) :: State s (a -> b) -> State s a -> State s b
  State stf <*> State stx = undefined

instance Monad (State s) where
  (>>=) :: State s a -> (a -> State s b) -> State s b
  State st >>= f = undefined

stackFoo :: State [Int] ()
stackFoo = do w <- pop
              x <- pop
              let wx = w * x
              y <- pop
              z <- pop
              let yz = y * z
              push $ wx + yz

-- Random numbers

randInRange :: (Int,Int) -> State StdGen Int
randInRange bounds = State $ randomR bounds

nRands :: Int -> (Int,Int) -> State StdGen [Int]
nRands 0 _ = return []
nRands n bounds = do x <- randInRange bounds
                     xs <- nRands (n-1) bounds
                     return (x:xs)

-- General get/put for reading/updating state

get :: State s s
get = State $ \s -> (s, s)

put :: a -> State a ()
put s = State $ \_ -> ((), s)

tick :: State Int ()
tick = do i <- get
          put (i+1)

statefulComp :: State Int (Int,Int)
statefulComp = do i <- get
                  tick
                  tick
                  tick
                  i' <- get
                  return (i, i')

-- newtype

newtype LoudInt  = Loud Int
newtype QuietInt = Quiet Int

instance Show LoudInt where
  show (Loud i) = "<<" ++ show (i-1) ++ "+" ++ "1>>"

instance Show QuietInt where
  show (Quiet i) = "(" ++ show i ++ ")"

-- IO

guess :: Int -> IO ()
guess n = do putStr "Enter a guess: "
             input <- getLine
             case reads input of -- reads tries to parse Int from input
               [(g,"")] -> do
                 if g < n then do
                   putStrLn "Too small!"
                   guess n
                 else if g > n then do
                   putStrLn "Too big!"
                   guess n
                 else
                   putStrLn "Good guess!"
               _ -> do -- parse failure
                putStrLn "Invalid input"
                guess n

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

