% CS 340: Programming Paradigms and Patterns
% Lect 10 - Some Monads
% Michael Lee

> module Lect.Lect10Complete where
> import Prelude hiding (sequence, sequence_, mapM, mapM_, 
>                        forM, forM_, interact)
> import Data.Char
> import Data.List
> import Data.Maybe


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

> noun_phrases = (++) <$> ["red ", "quick ", "fuzzy "] 
>                     <*> ["fox", "couch", "torpedo"]


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

> data Logger a = Logger {
>   loggerVal  :: a,
>   loggerMsgs :: [String]
> } deriving (Show)


As a functor, we should be able to apply functions to the value in a `Logger`:

> instance Functor Logger where
>   fmap f (Logger x l) = Logger (f x) l


When combining `Logger` values as applicative instances, we should combine the
log messages found in both `Logger`s:

> instance Applicative Logger where
>   pure x = Logger x []
>   (Logger f l1) <*> (Logger x l2) = Logger (f x) (l1 ++ l2)


Finally, let's define the monad instance:

> instance Monad Logger where
>   (Logger x l) >>= f = let (Logger y l') = f x
>                        in Logger y (l ++ l')


We need a few functions that produce `Logger` values:

> recordLog :: Show a => a -> String -> Logger a
> recordLog x s = Logger x [s]

> logVal :: Show a => a -> Logger a
> logVal x = recordLog x $ "Got " ++ show x
> 
> logOp :: Show a => String -> a -> Logger a
> logOp op x = recordLog x $ "Performing " ++ op ++ " => " ++ show x


What do the following evaluate to?

> logeg1 = return 5 >>= logVal

> logeg2 = do
>   a <- logVal 10
>   b <- logOp "Times 2" $ a*2
>   return b

> logeg3 = do
>   a <- logVal 5
>   b <- logVal 10
>   c <- logOp "Sub" $ a - b
>   d <- logOp "Square" $ c^2
>   return d


We may want functions to operate purely on log messages:

> logAppend :: String -> Logger ()
> logAppend l = recordLog () l


What do the following evaluate to?

> logeg4 = do
>   logAppend "Starting"
>   logAppend "Revving up"
>   logAppend "Warmed up"
>   return "Boom!"

> logeg5 = do
>   logAppend "Starting"
>   x <- logVal 10
>   logAppend "Revving up"
>   y <- logVal 20
>   logAppend "Warmed up"
>   z <- logOp "Add" $ x + y
>   return "Boom!"


State monad
-----------

Though Haskell doesn't allow for stateful functions, we can simulate the idea by
defining functions that take an input state and use it to compute a value,
returning that alongside an updated state. 

The `State` type represents just such a function:

> data State s a = State { runState :: s -> (s, a) }


We can define stateful functions that represent stack manipulations, where the
state is represented as a list and the computed value depends on the stack
operation semantics:

> pop :: State [a] a
> pop = State $ \(x:xs) -> (xs, x)

> push :: a -> State [a] ()
> push x = State $ \xs -> (x:xs, ())

> peek :: State [a] a
> peek = State $ \l@(x:xs) -> (l, x)


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

> instance Functor (State s) where
>   fmap f st = State $ \s -> let (s', x) = runState st s
>                             in (s', f x)

Then Applicative:

> instance Applicative (State s) where
>   pure x = State $ \s -> (s, x)
>   stf <*> stx = State $ \s -> let (s', f) = runState stf s
>                               in runState (f <$> stx) s'

And finally Monad:

> instance Monad (State s) where
>   st >>= f = State $ \s -> let (s', x) = runState st s
>                            in runState (f x) s'


Here's how we can use the State monad to chain together stack operations:

> stackArith :: State [Int] ()
> stackArith = do
>   w <- pop
>   x <- pop
>   let wx = w * x
>   y <- pop
>   z <- pop
>   let yz = y * z
>   push $ wx + yz

---

If we define a few functions that let us treat lists like mapping structures:

> get :: Eq a => a -> [(a,b)] -> b
> get x ((k,v):kvs) | x == k = v
>                   | otherwise = get x kvs

> put :: Eq a => a -> b -> [(a,b)] -> [(a,b)]
> put x y [] = [(x,y)]
> put x y ((kv@(k,_)):kvs) | x == k = (k,y):kvs
>                          | otherwise = kv : put x y kvs


We can write stateful functions that use them to store and update "variable" values:

> var_get :: String -> State [(String, a)] a
> var_get v = State $ \s -> (s, get v s)

> var_put :: String -> a -> State [(String, a)] ()
> var_put v x = State $ \s -> (put v x s, ())


And now we can chain together what looks like a simple imperative program:

> quadRoots :: State [(String, Double)] (Double, Double)
> quadRoots = do
>   a <- var_get "a"
>   b <- var_get "b"
>   c <- var_get "c"
>   var_put "disc" $ b^2 - 4*a*c
>   disc <- var_get "disc"
>   var_put "r1" $ (-b - sqrt disc) / (2*a)
>   var_put "r2" $ (-b + sqrt disc) / (2*a)
>   r1 <- var_get "r1"
>   r2 <- var_get "r2"
>   return (r1, r2)

---

-- Monadic utility functions

Now that we have the building blocks for writing stateful, sequential code, it
is useful to have "control structure" like mechanisms for expressing common
patterns.

A handy utility function for carrying out a bunch of monadic "actions" (as
represented by `State` monad values) is `sequence`:

> sequence :: Monad m => [m a] -> m [a]
> sequence [] = return []
> -- sequence (m:ms) = m >>= \x -> sequence ms >>= \xs -> return (x:xs)
> sequence (m:ms) = do x <- m
>                      xs <- sequence ms
>                      return $ x:xs
>
> -- applicatively:
> sequence' (m:ms) = (:) <$> m <*> sequence ms

What do the following give us?

> action1 = sequence [pop, pop, pop, pop, pop]
> action2 = sequence [push 1, push 2, push 3, push 4, push 5]


Sometimes we don't care about the result of a monadic action (e.g., for the
`push` operation), so we also have `sequence_`:

> sequence_ :: Monad m => [m a] -> m ()
> sequence_ [] = return ()
> -- sequence_ (m:ms) = m >> sequence_ ms
> sequence_ (m:ms) = do m
>                       sequence_ ms
>
> -- applicatively:
> sequence_' (m:ms) = (\_ _ -> ()) <$> m <*> sequence_ ms

And now we can do:

> action3 = sequence_ $ map push [1..5]


This pattern of mapping a function that returns a monad onto a list, then
sequencing it is so common that we implement `mapM` and `mapM_`

> mapM :: Monad m => (a -> m b) -> [a] -> m [b]
> mapM f = (sequence . map f)
>
> mapM_ :: Monad m => (a -> m b) -> [a] -> m ()
> mapM_ f = (sequence_ . map f)

Enabling:

> action4 = mapM_ push [1..5]


Finally, by flipping the order of arguments of `mapM` and `mapM_`, we have the
`forM` and `forM_` functions:

> forM :: Monad m => [a] -> (a -> m b) -> m [b]
> forM = flip mapM
>
> forM_ :: Monad m => [a] -> (a -> m b) -> m ()
> forM_ = flip mapM_

This lets us write code like this:

> action5 = forM_ [1..5] $ \x -> do
>               push x
>               push $ x*2
>               push $ x^2

Which looks an awful lot like a for loop!

---

Another example that helps drive home the power of monads is the implementation
of a tree-relabeling function.

Say we have a binary tree type:

> data Tree a = Node a (Tree a) (Tree a) | Leaf a deriving Show


Let's write a function that "relabels" the nodes in a tree using values pulled
from a list. For example, if we use the list "abcdefg" to relabel the following
tree, we should get back the tree where 1 is replaced with 'a', 2 with 'b', 3
with 'c', etc.

> t1 :: Tree Int
> t1 = Node 1 
>        (Node 2 
>          (Leaf 3)
>          (Leaf 4))
>        (Node 5 
>          (Leaf 6)
>          (Leaf 7))


Here's the start of an attempt:

> relabel :: Tree a -> [b] -> Tree b
> relabel (Leaf x) (y:ys) = Leaf y
> relabel (Node x l r) (y:ys) = undefined -- how to write this?


Implementing the recursive case is somewhat painful, as we need to apply as many
list values as necessary to relabel the left subtree, then apply the remaining
to relabel the right subtree ... but to do this we need to keep track of how
many values we "use up" from the list while recursing downwards. Possible,
certainly, but ugly!

The State monad to the rescue. Note how, in the following code, the monad takes
care of maintaining its own internal state. 

> relabel' :: Tree a -> State [b] (Tree b)
> relabel' (Leaf x) = do l <- pop
>                        return $ Leaf l
> 
> relabel' (Node x l r) = do y <- pop
>                            l' <- relabel' l
>                            r' <- relabel' r
>                            return $ Node y l' r'


To relabel the tree above, we need simply do:

    runState (relabel' t1) "abcdefg"


Note that we could also have implemented relabel using the applicative style:

> relabel'' :: Tree a -> State [b] (Tree b)
> relabel'' (Leaf x) = Leaf <$> pop
> relabel'' (Node x l r) = Node <$> pop <*> relabel' l <*> relabel' r


IO monad
--------

The IO monad is conceptually defined very similarly to the State monad:

    data IO a = IO (RealWorld -> (RealWorld, a))

RealWorld replaces the arbitrary state type, but otherwise the IO type also
represents a polymorphic function that takes an input state and returns a tuple
containing the output state and a value.

There are a few more critical differences: 

  1. We cannot directly access the function contained within an IO value in
     order to apply it to an argument explicitly. A consequence of this is that
     once a value or computation is placed inside an IO monad, we can never
     unwrap it --- i.e., it will forever be marked as an IO operation!
     
     (If we wished to emulate this behavior for the State monad, we could've
     hidden the `State` value constructor and the `runState` accessor function
     within its own module, while still exporting functions that return/accept
     State monads.)
     
  2. We cannnot create RealWorld values! The real world is stateful and
     impure, so it has no place in Haskell programs.
     
     But if we have an IO value, how do we ever apply it to an input?

     We don't! (At least not from within a Haskell program.)

Here's the final piece of the puzzle -- the main function:

    main :: IO ()

`main` is defined as an IO monad value (aka an IO "action") that returns
nothing. When we compile our program and execute it (e.g., from the command
line), this action is "run" on a RealWorld.

Since this is the only place in our programs where we are handed a RealWorld
value, every other IO action we wish to perform must tie into the stateful
computation ultimately returned from `main`. We accomplish this by way of the
applicative and monad operations.

---

The Prelude and System.IO libraries define a bunch of functions that return IO
actions. Here are some:

    getLine :: IO String
    readFile :: FilePath -> IO String
    readLn :: Read a => IO a
    putStrLn :: String -> IO ()
    print :: Show a => a -> IO ()
    interact :: (String -> String) -> IO ()

  - Consider `getLine`, for instance. It is defined as an IO action that
    "contains" a string. This means it is a stateful function which, when handed
    a RealWorld, extracts and returns a string from it alongside an updated
    RealWorld value that can be handed to another IO action. 

  - `putStrLn`, on the other hand, takes a string and returns an IO action
    that contains nothing. This is presumably because the stateful function
    represented by `putStrLn` only affects change on its RealWorld argument
    instead of extracting anything useful from it. 

We can sequence these two actions together like this:

> main :: IO ()
> main = do name <- getLine
>           putStrLn $ "Hello, " ++ name

Normally, we would place this definition in the file "Main.hs" (located in the
"app" directory, in our repository), then use the command "stack run" to build
and execute the program. But we can also use GHCi to perform IO actions directly
(i.e., when an IO action is evaluated in GHCi, it is immediately run on the
RealWorld and we can see the results).

Here's a function that implements a guessing game:

> guess :: Int -> IO ()
> guess n = do putStr "Enter a guess: "
>              x <- readLn
>              case compare x n of 
>                  LT -> do putStrLn "Too small!"
>                           guess n
>                  GT -> do putStrLn "Too big!"
>                           guess n
>                  otherwise -> do putStrLn "Good guess!"
>                                  return ()

Notice how we're basically constructing a huge chain of IO actions via the bind
operator (which is hidden by `do` notation). Also, note how we use recursion to
perform "loops" --- this is ok because in Haskell recursing doesn't consume any
stack space, due to lazy evaluation!

---

We can also apply pure functions to values contained within IO actions. 

E.g., let's write an IO action that reads a line, applies a Caesar cipher with a
shift of 5 to it, then prints out the encrypted value:

> caesar :: Int -> String -> String
> caesar _ [] = []
> caesar n (x:xs) = (if isLetter x then encrypt x else x) : caesar n xs
>   where encrypt x = n2l ((l2n x + n) `mod` 26)
>         l2n c = ord (toUpper c) - ord 'A'
>         n2l n = chr (n + ord 'A')
>
>
> caesarAction :: IO ()
> caesarAction = do s <- getLine
>                   putStrLn $ caesar 5 s

We can also implement it using applicative/monadic operators explicitly:

> caesarAction' :: IO ()
> caesarAction' = (caesar 5 <$> getLine) >>= putStrLn


This pattern of building an IO action around a function that takes a string and
returns a string is so common that we have `interact`

> interact :: (String -> String) -> IO ()
> interact f = do s <- getContents -- getContents returns the entire input
>                 putStr $ f s

which we can use to write interactive command line tools! (`interact` doesn't
behave well in GHCi, so we'll have to try this out in "Main.hs")

Update "Main.hs" so that it implements a program which breaks its input up into
lines, reverses each line (for additional security) and applies the Caesar
cipher with a shift width of 5 to it, then prints out the encrypted lines.

(Hint: look into the `lines` and `unlines` functions).

Ans: 

    main = interact $ unlines . map (caesar 5 . reverse) . lines


Q/A
---

Q: Did we just reinvent the (imperative) wheel?

A: We did something much more general! A monad lets its designer decide exactly
   how actions are sequenced together, which is not the case for how statements
   are sequenced in an imperative language (that semantic is baked in). 


Q: Doesn't having IO actions in a Haskell program break functional purity?

A: Nope. The key is that we don't actually *perform* any IO actions in our
   programs. A function that returns an IO action (which represents a stateful
   computation) is still pure, in that it will alway return the same value for
   the given argument(s). IO actions are only ever performed at runtime, when we
   execute our programs. This allows us to continue reasoning about our code in
   a purely functional way!


Q: But if an otherwise pure function extracts the value from an IO action and
   uses it in a computation, doesn't that make the function effectively
   stateful?

A: Remember, it is impossible to unwrap IO actions! If a pure function is
   applied to the value in an IO action, its return value cannot be extracted
   from the resulting IO action. What this means is that any function that needs
   to "do work" inside an IO action will itself be forced to return an IO action
   (which, again, doesn't actually get performed until runtime).

   E.g., the following function takes a pure function f and applies it to the
   contents of an IO action ... is there any way to not return the result in an
   IO action?

       foo :: (a -> b) -> IO a -> IO b
       foo f m = f <$> m
   

Q: Couldn't lazy evaluation wreak havoc on programs that perform IO? E.g., if a
   program reads three different values from a file, but lazy evaluation causes
   the last read to be evaluated before the first two, the program would likely
   produce an incorrect result!

A: Firstly, remember that all IO actions to be performed at runtime by a
   program must be chained together in an unbroken sequence starting at `main`,
   as that is the only place where we are handed a RealWorld value at runtime.

   Secondly, all the IO actions in this chain are ultimately glued together
   using either the elementary applicative or monadic combinators (i.e., <*>,
   >>=, and >>). These combinators are designed to make the dependencies between
   actions clear, and -- in the case of the IO monad -- to guarantee sequential execution!

   E.g., our State monad makes dependencies between actions clear:

       instance Monad (State s) where
         st >>= f = State $ \s -> let (s', x) = runState st s
                                  in runState (f x) s'


Q: Do all monads impose strictly sequential evaluation, and thereby prevent
   us from taking advantage of Haskell's laziness?

A: No! The IO monad does this out of necessity, but monads don't need to behave
   this way. 
   
   What it means to sequence monadic values depends on the implementation of
   bind (>>=), which, as we've seen, depends on the structure and interpretation
   of the associated monad instance. 
   
   While sequencing monadic values may seem to imply certain dependencies 
   (e.g., `m >>= f >>= g` implies that `g` depends on the result of `f`, which
   depends on `m`), Haskell will continue to evaluate only those expressions
   that are needed, when they are needed.

   E.g., despite the functional dependency implied in the sequencing of `foo`
   and `bar` within `baz`, running `baz` does not result in an exception, as
   `bar` requires neither `x` nor its input state to produce a result:

       foo :: State Int Int
       foo = State undefined
      
       bar :: Int -> State Int Int
       bar _ = State $ \_ -> (1,2)
      
       baz = do x <- foo
                y <- bar x
                return y
