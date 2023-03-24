% CS 340: Programming Paradigms and Patterns
% Lect 09 - Functors, Applicatives, and Monads
% Michael Lee

\begin{code}
module Lect09 where
import Prelude hiding (Functor, fmap, (<$>),
                       Applicative, pure, (<*>),
                       Monad, (>>=), (>>), return)
import Data.List hiding (find)
\end{code}


Functors, Applicatives, and Monads
==================================

Agenda:
  - Functors
  - Applicatives
  - Monads
  - Laws


Functors
--------

Functors are a class of types that support a "mapping" operation. An instance of
functor must be a polymorphic type, as a type variable will be used to repesent
the target of the mapped function. Here's the `Functor` class:

\begin{code}
class Functor f where
  fmap :: (a -> b) -> f a -> f b
\end{code}

Note that the kind of type `f` implied by the class definition above is 
`* -> *`, as `f a` and `f b` in the type definition of `fmap` use `f` as a type
constructor.

---

We can make a list a Functor, where `fmap` is identical to `map`

\begin{code}
instance Functor [] where
  fmap :: (a -> b) -> [a] -> [b]
  fmap = undefined
\end{code}

Note that the implied type of `fmap` in the instance (replacing `f` from the
class definition with the instance `[]`) is:

    fmap :: (a -> b) -> [a] -> [b]

Which is exactly what we want.

We can now do

    fmap (2*) [1..10]

to map the function (2*) over the values in the list.

---

Let's define `fmap` for the Maybe type:

\begin{code}
instance Functor Maybe where
  fmap :: (a -> b) -> Maybe a -> Maybe b
  fmap = undefined
\end{code}

we can now do:

\begin{verbatim}
  fmap (2*) Just 10
  
  fmap (100^) Nothing
\end{verbatim}


If we consider `Nothing` to be the result of a failed computation, it makes
sense that `fmap`-ing over it also results in `Nothing`.     

---

We can define an infix form of `fmap`:

\begin{code}
infixl 4 <$>
(<$>) :: Functor f => (a -> b) -> f a -> f b
(<$>) = fmap
\end{code}

Think of this operator as representing function application to values in an 
arbitrary context, where the context is represented by a functor instance. 

E.g.,

\begin{verbatim}
  reverse <$> ["Hello", "how", "are", "you"]
  
  reverse <$> Just "Aloha"
  
  reverse <$> Nothing
\end{verbatim}

---

Let's define `fmap` for a tree:

\begin{code}
data Tree a = Node a [Tree a] | Leaf a

instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap = undefined
\end{code}


Here's a `Show` instance that makes it easier to visualize trees and a few trees
to play with:

\begin{code}
-- Show instance below is to make visualizing tree changes easier!
instance (Show a) => Show (Tree a) where
  show :: Show a => Tree a -> String
  show t = s 0 t
    where s n (Leaf x) = replicate n '.' ++ show x ++ "\n"
          s n (Node x t) = replicate n '.'
                               ++ show x ++ "\n"
                               ++ concat (map (s (n+2)) t)

t1 :: Tree String
t1 = Node "Animals" [
       Leaf "Arthropods", 
       Node "Chordates" [ Leaf "Birds", Leaf "Mammals", Leaf "Reptiles" ],
       Leaf "Nematodes"
     ]

t2 :: Tree Integer
t2 = Node 8 [
       Node 5 [ 
           Node 2 [Leaf 1, Leaf 1], 
           Node 3 [Leaf 1, 
                   Node 2 [Leaf 1, Leaf 1] ]
       ],
       Node 3 [Leaf 1, 
                Node 2 [Leaf 1, Leaf 1] ]
     ]
\end{code}

---

Even functions can be thought of as Functors! A function with type `a -> b` is a
computation that takes a type `a` and eventually produces a type `b`; i.e., it
is a computational context for a value of type `b`.

The `->` operator itself is a type constructor with kind `* -> * -> *`, so we
can write a Functor instance for the type `((->) a)` (note that `a` is the first
/ left-hand argument to the type constructor `->`):

\begin{code}
instance Functor ((->) a) where
  fmap :: (b -> c) -> (a -> b) -> (a -> c)
  fmap = undefined
\end{code}

How should we interpret the following?

\begin{verbatim}
  ((2*) <$> (5+) <$> (100-)) 90    
\end{verbatim}

---

How are functors limited?


Applicative Functors
--------------------

The Applicative class extends Functors with additional methods. "pure" takes a
value and wraps it in a Functor instance, while "<*>" applies a function found
in one Functor to a value in another Functor.

\begin{code}
class (Functor f) => Applicative f where
  pure :: a -> f a
  
  infixl 4 <*>
  (<*>) :: f (a -> b) -> f a -> f b
\end{code}

---

Let's make `Maybe` an Applicative instance:

\begin{code}
instance Applicative Maybe where
  pure :: a -> Maybe a
  pure = undefined
  
  (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
  (<*>) = undefined
\end{code}


Now we can do:

\begin{verbatim}
  pure (2*) <*> pure 5

  pure ("hello" ++ ) <*> Just "world"

  (*) <$> Just 2 <*> Just 5

  (\x y z -> x++y++z)  <$> Just "hello" <*> Just "hola" <*> Just "hi"
\end{verbatim}


More interestingly, if any of the arguments are `None`, the Applicative instance
handles it correctly:

\begin{verbatim}
  (*) <$> Nothing <*> Just 5

  (\x y z -> x++y++z)  <$> Just "hello" <*> Nothing <*> Just "hi"
\end{verbatim}

---

How do we make a list an applicative? I.e., what sort of context does a list
represent?

\begin{code}
instance Applicative [] where
  pure :: a -> [a]
  pure = undefined
  
  (<*>) :: [a -> b] -> [a] -> [b]
  (<*>) = undefined
\end{code}

Try:

\begin{verbatim}
  [(2^), (5+), (3*)] <*> [5..7]
\end{verbatim}

A more interesting approach would be to think of a list as representing a
*non-deterministic context*. (What does this mean?)

We can't define multiple instances of a class for a given type, so we use
`newtype` to create a type from a list:

\begin{code}
newtype NDList a = NDList [a] deriving Show
\end{code}

Now we can make `NDList` an instance of Functor and Applicative:

\begin{code}
instance Functor NDList where
  fmap :: (a -> b) -> NDList a -> NDList b
  fmap = undefined

instance Applicative NDList where
  pure :: a -> NDList a
  pure = undefined
  
  (<*>) :: NDList (a -> b) -> NDList a -> NDList b
  (<*>) = undefined
\end{code}


Try:

\begin{verbatim}
  NDList [("Hello "++)] <*> NDList ["Michael", "Mary", "Beth"]

  NDList [(2^), (5+), (3*)] <*> NDList [5..7]

  NDList [(,)] <*> NDList [1..10] <*> NDList "hello"
\end{verbatim}

The built-in Applicative list instance is based on this non-deterministic
interpretation.

---

We can also make a function an Applicative:

\begin{code}
instance Applicative ((->) a) where
  pure :: b -> (a -> b)
  pure = undefined

  (<*>) :: (a -> (b -> c)) -> (a -> b) -> (a -> c)
  (<*>) = undefined
\end{code}

What does this do?

\begin{verbatim}
  (zip <*> drop 5) [1..10]
\end{verbatim}

---

Applicatives are handy for when we need to apply "pure" functions to values in
contexts represented by Functors. A pure function is one that does not wrap its
return value in a context. 

E.g., 

\begin{verbatim}
  even <$> [1..10]

  (+) <$> Just 5 <*> Just 10

  (++) <$> ["hello, ", "hola, "] <*> ["mister", "senor"]

  (++) <$> NDList ["hello, ", "hola, "] <*> NDList ["mister", "senor"]
\end{verbatim}

But what happens when we want to apply an "impure" function --- i.e., one that
returns its own context --- to a value in a Functor? 

E.g., recall the `find` function:

\begin{code}
find :: (a -> Bool) -> [a] -> Maybe a
find _ [] = Nothing
find p (x:xs) | p x = Just x
              | otherwise = find p xs
\end{code}

What happens when we do the following?

\begin{verbatim}
  find even <$> Just [1..10]

  find (>100) <$> Just [1..100]
\end{verbatim}

What would be preferable?


Monads
------

The Monad class further extends Applicatives so that they support the additional
methods `>>=` ("bind"), `>>` ("sequence"), and `return`:

\begin{code}
class (Applicative m) => Monad m where
  infixl 1 >>=
  (>>=) :: m a -> (a -> m b) -> m b -- called "bind"

  infixl 1 >>
  (>>) :: m a -> m b -> m b -- called "sequence"
  x >> y = x >>= \_ -> y

  return :: a -> m a
  return = pure
\end{code}

---

Let's make `Maybe` a Monad instance:

\begin{code}
instance Monad Maybe where
  (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
  (>>=) = undefined
\end{code}

Now we get sensible results by doing:

\begin{verbatim}
  Just [1..10] >>= find even

  Just [1..10] >>= find (>100)
\end{verbatim}

---

For a better example of how this is useful, consider `div`, defined in Prelude:

    div :: Integral a => a -> a -> a

`div` throws an exception if the denominator is 0. We can improve on this by
writing `safeDiv` as follows:

\begin{code}
safeDiv :: Integral a => a -> a -> Maybe a
safeDiv _ 0 = Nothing
safeDiv x y = Just $ x `div` y
\end{code}

Imagine that we need to implement a function that computes the following with
integral operands:

                          (a / b) + (c / d)
    fDivs a b c d e f  =  ----------------- * f
                                  e

Without bind, we might write:

\begin{code}
fDivs :: Integral a => a -> a -> a -> a -> a -> a -> Maybe a
fDivs a b c d e f = undefined
\end{code}

Or we can use our bind operator:

\begin{code}
fDivs' :: Integral a => a -> a -> a -> a -> a -> a -> Maybe a
fDivs' a b c d e f = undefined
\end{code}

Explain how this works!

---

The lambda chaining is such a common pattern that Haskell provides special
syntax for doing this -- "do notation":

\begin{code}
fDivs'' :: Integral a => a -> a -> a -> a -> a -> a -> Maybe a
fDivs'' a b c d e f = do r   <- a `safeDiv` b
                         r'  <- c `safeDiv` d
                         r'' <- (r + r') `safeDiv` e
                         return $ r'' * f
\end{code}

If a line in a `do` block does not use the `<-` operator, then it and the
following line are combined using the `>>` (sequence) operator. For example,
translate the following `do` block to lambda notation using `>>=` and `>>`:

    do r1 <- func1 x
       r2 <- func2 y
       func3 r1 r2
       r3 <- func4 z
       func5 r3
       return (r1, r2, r3)

`do` blocks also support `let` definitions, but `in` is omitted, e.g.:

    do r1 <- func1 w
       r2 <- func2 x
       let y = pureFunc r1 r2
           z = pureVal
       r3 <- func3 y z
       return (r1, r2, r3)


Laws
----

Properly implemented instances of Functors, Applicatives, and Monads should
conform to a handful of laws. These laws exist to ensure that all class
instances behave in a predictable way. The compiler does not, however, enforce
these laws for us!

-- Functor laws

  1. Identity:
    
       fmap id = id

  2. Composition:

       fmap (f . g) = (fmap f . fmap g)

-- Applicative laws

  1. Identity:

       pure id <*> v = v

  2. Homomorphism:

       pure f <*> pure x = pure (f x)

  3. Interchange:

       u <*> pure x = pure ($ x) <*> u

  4. Composition:

       pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
    
-- Monad laws

  1. Left Identity:

       return x >>= f = f x

     or equivalently: 
     
       do { y <- return x; f y } = do { f x }

  2. Right Identity:

       m >>= return = m

     or equivalently:

       do { x <- m; return x } = do { m }

  3. Associativity:

       (m >>= \x -> f x) >>= g = m >>= (\x -> f x >>= g)
    
     or equivalently:

       do                     do
         y <- do x <- m   =     x <- m
                 f x            y <- f x
         g y                    g y
