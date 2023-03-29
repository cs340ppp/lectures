% CS 340: Programming Paradigms and Patterns
% Lect 10 - Some Monads
% Michael Lee

\begin{code}
module Lect10 where
import Prelude hiding (sequence, sequence_, mapM, mapM_, 
                       forM, forM_, interact)
import Data.Char
import Data.List
import Data.Maybe
\end{code}


Some Monads
===========

Agenda:
  - List
  - Logger
  - State
    - Monadic utility functions
  - IO
  - Q/A


List monad
----------

Recall that the built-in applicative instance of the list type uses the
non-deterministic intepretation of the applicative `<*>` operator:

\begin{code}
noun_phrases = (++) <$> ["red ", "quick ", "fuzzy "] 
                    <*> ["fox", "couch", "torpedo"]
\end{code}


The list monad instance is consistent with this interpretation:

    instance Monad [] where
      xs >>= f = concat [f x | x <- xs]


What do the following evaluate to?

    [1..10] >>= return

    [1..10] >>= \x -> replicate x x

    do x <- [1..10]
       y <- "hello"
       return (x,y)

    do article <- ["The", "A", "This"]
       adjective <- ["red", "quick", "fuzzy"]
       noun <- ["fox", "couch", "torpedo"]
       return $ article ++ " " ++ adjective ++ " " ++ noun


In some cases the use of bind can be replaced with pure functions and the `<$>`
and `<*>` operators. This is not always the case, though!


Logger monad
------------

The Logger monad is designed to provide a mechanism for attaching log messages
to values and computations. The log messages are automatically (via the
bind/sequence operators) combined and accumulated throughout sequences of
monadic operations.

We start by defining a type that encapsulates a polymorphic value and a list of
log messages:

\begin{code}
data Logger a = Logger {
  loggerVal  :: a,
  loggerMsgs :: [String]
} deriving (Show)
\end{code}


As a functor, we should be able to apply functions to the value in a `Logger`:

\begin{code}
instance Functor Logger where
  fmap = undefined
\end{code}


When combining `Logger` values as applicative instances, we should combine the
log messages found in both `Logger`s:

\begin{code}
instance Applicative Logger where
  pure = undefined
  (<*>) = undefined
\end{code}


Finally, let's define the monad instance:

\begin{code}
instance Monad Logger where
  (>>=) = undefined
\end{code}


We need a few functions that produce `Logger` values:

\begin{code}
recordLog :: Show a => a -> String -> Logger a
recordLog x s = Logger x [s]

logVal :: Show a => a -> Logger a
logVal x = recordLog x $ "Got " ++ show x
 
logOp :: Show a => String -> a -> Logger a
logOp op x = recordLog x $ "Performing " ++ op ++ " => " ++ show x
\end{code}


What do the following evaluate to?

\begin{code}
logeg1 = return 5 >>= logVal

logeg2 = do
  a <- logVal 10
  b <- logOp "Times 2" $ a*2
  return b

logeg3 = do
  a <- logVal 5
  b <- logVal 10
  c <- logOp "Sub" $ a - b
  d <- logOp "Square" $ c^2
  return d
\end{code}


We may want functions to operate purely on log messages:

\begin{code}
logAppend :: String -> Logger ()
logAppend l = recordLog () l
\end{code}


What do the following evaluate to?

\begin{code}
logeg4 = do
  logAppend "Starting"
  logAppend "Revving up"
  logAppend "Warmed up"
  return "Boom!"

logeg5 = do
  logAppend "Starting"
  x <- logVal 10
  logAppend "Revving up"
  y <- logVal 20
  logAppend "Warmed up"
  z <- logOp "Add" $ x + y
  return "Boom!"
\end{code}


State monad
-----------

Though Haskell doesn't allow for stateful functions, we can simulate the idea by
defining functions that take an input state and use it to compute a value,
returning that alongside an updated state. 

The `State` type represents just such a function:

\begin{code}
data State s a = State { runState :: s -> (s, a) }
\end{code}


We can define stateful functions that represent stack manipulations, where the
state is represented as a list and the computed value depends on the stack
operation semantics:

\begin{code}
pop :: State [a] a
pop = undefined

push :: a -> State [a] ()
push x = undefined

peek :: State [a] a
peek = undefined
\end{code}


We can now run these operations like so on input lists:

    runState pop [1..10]

    runState (push 5) [1..10]

    runState peek [1..10]


More intriguingly, we can chain them, like this:

    let s1 = []
    in let (s2, _) = runState (push 5) s1
       in let (s3, _) = runState (push 7) s2
          in let (s4, x) = runState pop s3
             in let (s5, y) = runState pop s4
                in x+y

But this is really, really ugly. Monads exist to help us get rid of all the
manual chaining. So let's make a Monad out of State!

---

Start with Functor:

\begin{code}
instance Functor (State s) where
  fmap f st = undefined
\end{code}

Then Applicative:

\begin{code}
instance Applicative (State s) where
  pure = undefined
  stf <*> stx = undefined
\end{code}

And finally Monad:

\begin{code}
instance Monad (State s) where
  st >>= f = undefined
\end{code}


Here's how we can use the State monad to chain together stack operations:

\begin{code}
stackArith :: State [Int] ()
stackArith = do
  w <- pop
  x <- pop
  let wx = w * x
  y <- pop
  z <- pop
  let yz = y * z
  push $ wx + yz
\end{code}

---

If we define a few functions that let us treat lists like mapping structures:

\begin{code}
get :: Eq a => a -> [(a,b)] -> b
get x ((k,v):kvs) | x == k = v
                  | otherwise = get x kvs

put :: Eq a => a -> b -> [(a,b)] -> [(a,b)]
put x y [] = [(x,y)]
put x y ((kv@(k,_)):kvs) | x == k = (k,y):kvs
                         | otherwise = kv : put x y kvs
\end{code}


We can write stateful functions that use them to store and update "variable" values:

\begin{code}
var_get :: String -> State [(String, a)] a
var_get v = undefined

var_put :: String -> a -> State [(String, a)] ()
var_put v x = undefined
\end{code}


And now we can chain together what looks like a simple imperative program:

\begin{code}
quadRoots :: State [(String, Double)] (Double, Double)
quadRoots = do
  a <- var_get "a"
  b <- var_get "b"
  c <- var_get "c"
  var_put "disc" $ b^2 - 4*a*c
  disc <- var_get "disc"
  var_put "r1" $ (-b - sqrt disc) / (2*a)
  var_put "r2" $ (-b + sqrt disc) / (2*a)
  r1 <- var_get "r1"
  r2 <- var_get "r2"
  return (r1, r2)
\end{code}

---

-- Monadic utility functions

Now that we have the building blocks for writing stateful, sequential code, it
is useful to have "control structure" like mechanisms for expressing common
patterns.

A handy utility function for carrying out a bunch of monadic "actions" (as
represented by `State` monad values) is `sequence`:

\begin{code}
sequence :: Monad m => [m a] -> m [a]
sequence [] = return []
sequence (m:ms) = undefined
\end{code}


What do the following give us?

\begin{code}
action1 = sequence [pop, pop, pop, pop, pop]
action2 = sequence [push 1, push 2, push 3, push 4, push 5]
\end{code}


Sometimes we don't care about the result of a monadic action (e.g., for the
`push` operation), so we also have `sequence_`:

\begin{code}
sequence_ :: Monad m => [m a] -> m ()
sequence_ [] = return ()
sequence_ (m:ms) = undefined
\end{code}


And now we can do:

\begin{code}
action3 = sequence_ $ map push [1..5]
\end{code}


This pattern of mapping a function that returns a monad onto a list, then
sequencing it is so common that we implement `mapM` and `mapM_`

\begin{code}
mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f = undefined

mapM_ :: Monad m => (a -> m b) -> [a] -> m ()
mapM_ f = undefined
\end{code}

Enabling:

\begin{code}
action4 = mapM_ push [1..5]
\end{code}


Finally, by flipping the order of arguments of `mapM` and `mapM_`, we have the
`forM` and `forM_` functions:

\begin{code}
forM :: Monad m => [a] -> (a -> m b) -> m [b]
forM = undefined

forM_ :: Monad m => [a] -> (a -> m b) -> m ()
forM_ = undefined
\end{code}

This lets us write code like this:

\begin{code}
action5 = forM_ [1..5] $ \x -> do
              push x
              push $ x*2
              push $ x^2
\end{code}

Which looks an awful lot like a for loop!

---

Another example that helps drive home the power of monads is the implementation
of a tree-relabeling function.

Say we have a binary tree type:

\begin{code}
data Tree a = Node a (Tree a) (Tree a) | Leaf a deriving Show
\end{code}


Let's write a function that "relabels" the nodes in a tree using values pulled
from a list. For example, if we use the list "abcdefg" to relabel the following
tree, we should get back the tree where 1 is replaced with 'a', 2 with 'b', 3
with 'c', etc.

\begin{code}
t1 :: Tree Int
t1 = Node 1 
       (Node 2 
         (Leaf 3)
         (Leaf 4))
       (Node 5 
         (Leaf 6)
         (Leaf 7))
\end{code}


Let's implement this directly:

\begin{code}
relabel :: Tree a -> [b] -> Tree b
relabel = undefined
\end{code}


Let's try to write this using the State monad:

\begin{code}
relabel' :: Tree a -> State [b] (Tree b)
relabel' = undefined
\end{code}


IO monad
--------

The IO monad is conceptually defined very similarly to the State monad:

    data IO a = IO (RealWorld -> (RealWorld, a))

It looks similar to the State monad, with a few critical differences: 

  1. We cannot directly access the function contained within an IO value in
     order to apply it to an argument explicitly. A consequence of this is that
     once a value or computation is placed inside an IO monad, we can never
     unwrap it --- i.e., it will forever be marked as an IO operation!
     
  2. We cannnot create RealWorld values! The real world is stateful and
     impure, so it has no place in Haskell programs. So how and when do IO
     actions get performed?

Here's the final piece of the puzzle -- the main function:

    main :: IO ()

`main` is the only place where a RealWorld value is handed to our program, and
this only happens at runtime.

---

The Prelude and System.IO libraries define a bunch of functions that return IO
actions. Here are some:

    getLine :: IO String
    readFile :: FilePath -> IO String
    readLn :: Read a => IO a
    putStrLn :: String -> IO ()
    print :: Show a => a -> IO ()
    interact :: (String -> String) -> IO ()

  - How do we interpret `getLine`? `putStrLn`?


We can sequence these two actions together like this:

\begin{code}
main :: IO ()
main = do name <- getLine
          putStrLn $ "Hello, " ++ name
\end{code}


Let's write a function that implements a guessing game:

\begin{code}
guess :: Int -> IO ()
guess n = undefined
\end{code}


---

We can also apply pure functions to values contained within IO actions. 

E.g., let's write an IO action that reads a line, applies a Caesar cipher with a
shift of 5 to it, then prints out the encrypted value:

\begin{code}
caesar :: Int -> String -> String
caesar _ [] = []
caesar n (x:xs) = (if isLetter x then encrypt x else x) : caesar n xs
  where encrypt x = n2l ((l2n x + n) `mod` 26)
        l2n c = ord (toUpper c) - ord 'A'
        n2l n = chr (n + ord 'A')

caesarAction :: IO ()
caesarAction = undefined
\end{code}


This pattern of building an IO action around a function that takes a string and
returns a string is so common that we have `interact`

\begin{code}
interact :: (String -> String) -> IO ()
interact f = do s <- getContents -- getContents returns the entire input
                putStr $ f s
\end{code}


Update "Main.hs" so that it implements a program which breaks its input up into
lines, reverses each line (for additional security) and applies the Caesar
cipher with a shift width of 5 to it, then prints out the encrypted lines.

(Hint: look into the `lines` and `unlines` functions).


Q/A
---

Q: Did we just reinvent the (imperative) wheel?

Q: Doesn't having IO actions in a Haskell program break functional purity?

Q: But if an otherwise pure function extracts the value from an IO action and
   uses it in a computation, doesn't that make the function effectively
   stateful?

   E.g., is there any way to not return the result in an IO action?

       foo :: (a -> b) -> IO a -> IO b
       foo f m = f <$> m   

Q: Couldn't lazy evaluation wreak havoc on programs that perform IO? E.g., if a
   program reads three different values from a file, but lazy evaluation causes
   the last read to be evaluated before the first two, the program would likely
   produce an incorrect result!

Q: Do all monads impose strictly sequential evaluation, and thereby prevent
   us from taking advantage of Haskell's laziness?

   E.g., is `baz` strictly evaluated?

       foo :: State Int Int
       foo = State undefined
      
       bar :: Int -> State Int Int
       bar _ = State $ \_ -> (1,2)
      
       baz = do x <- foo
                y <- bar x
                return y
