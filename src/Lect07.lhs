% CS 340: Programming Paradigms and Patterns
% Lect 07 - Higher order functions
% Michael Lee

> module Lect07 where
> import Prelude hiding (foldr, foldl, foldr1, foldl1)
> import Data.Char

Higher order functions
======================

Agenda:
  - HOF overview
  - Functions as values
    - Named functions
    - Partial application
    - Operator sections
    - Lambda expressions
  - Function application
  - Function composition
  - Map
  - Filter
  - Fold


HOF overview
------------

A higher-order function is a function that takes a function as a parameter or
returns a function. ("Regular" functions are called first-order functions).

In many cases, HOFs create abstractions for commonly used implementation
patterns (e.g., iteration, filtering, folding) so that we can reuse them while
swapping in just the bits that matter (e.g., what to compute in each iteration,
what predicate to use for filtering). 

HOFs are a fundamental tool in functional programming.


Functions as values
-------------------

To use or implement HOFs we need to create function values; there are numerous
ways of doing this.


-- Named functions

When we define functions at the top-level or in where/let clauses, their names
are just symbols bound to function values:

> foo :: Int -> Int
> foo = undefined
> 
> bar :: Int -> Int
> bar = foo


-- Partial application

Due to currying, *all* functions of two or more arguments are HOFs. Partially
applying a function returns a function of fewer arguments.

Consider:

> strCat :: String -> String -> String
> strCat s t = s ++ t
>
> sayHiTo :: String -> String
> sayHiTo = strCat "hi, "


-- Operator sections

We can also partially apply operators by placing them in parentheses and
leaving out one of their arguments, which give us partial functions "waiting
for" the missing argument.

E.g.,

> twoPlus = (2+)
> divByThree = (/3)
> divThreeBy = (3/)
> sayHiTo' = ("hi, " ++)


-- Lambda expressions

Lambda expressions allow us to define anonymous functions.

Syntax: \ var1 var2 ... -> expression

  Note: the expression after `->` extends as far to the right as possible,
        so parenthesization is often needed!

We can use it as an alternative way to define top-level functions:

> sumOfSquares :: Num a => a -> a -> a
> sumOfSquares = \x y -> x^2 + y^2


We often use a lambda when we want to make it clear that a function is designed
to return a function:

> greeter :: String -> (String -> String) -- note: parens are superfluous
> greeter g = \name -> g ++ ", " ++ name

> sayHiTo'' = greeter "hi"


We also use lambdas to create simple functions to be passed to HOFs:

E.g., the library function `dropWhile` is the HOF version of drop:

    dropWhile :: (a -> Bool) -> [a] -> [a]

So we can do:

    dropWhile (\x -> isDigit x || isPunctuation x) "01. Lorem ipsum"


Function application
--------------------

Function application is defined via the `$` operator, thusly:

    ($) :: (a -> b) -> a -> b
    infixr 0 $
    f $ x = f x


The low(est) precedence and right associativity of `$` make it so we can often
use it to avoid the excessive parenthesization that would be necessary with
default function application (via space).

E.g., how can we rewrite the following using `$`?

    sayHiTo ("Michael" ++ "Lee")

    show (abs (2 - 5))

    take 5 (drop 10 (zip [1..] (repeat 'a')))


Function composition
--------------------

Function composition is an operation on two functions that applies the first to
the result of applying the second. It is defined thusly:

    (.) :: (b -> c) -> (a -> b) -> a -> c
    infixr 9 .
    f . g = \x -> f (g x)


`.` lets us succinctly combine functions:

    (sqrt . sin) 1
    (take 10 . repeat) 5


`.` also facilitates "point-free" (i.e., argument-less) function definitions.

E.g., re-implement `even'`, `k2h`, and `strip` with `.`

> even' :: Integral a => a -> Bool
> even' x = (x `rem` 2) == 0
>
>
> k2c k = k - 273
> c2f c = c * 9 / 5 + 32
> f2h f
>   | f < 0 = "too cold"
>   | f > 100 = "too hot"
>   | otherwise = "survivable"
>
>
> k2h  k = f2h $ c2f $ k2c k
>
>
> strip :: String -> String
> strip s = reverse $ dropWhile isSpace $ reverse $ dropWhile isSpace s


`.` also often does away with the need to create lambdas. 

E.g., how can we do away with the lambdas in the following?

    dropWhile (\w -> length w > 3) $ words "hello there, how are you?"


Map
---

`map` applies a function to each item of a list, returning the new list.

    map :: (a -> b) -> [a] -> [b]


Try out:

    map even [1..10]

    map reverse $ words "madam I refer to adam"

    map (^2) [1..10]

    map (\x->(x,x^2)) [1..10]

    map (++) $ words "on over under across" -- what does this do?

    map (\f -> f " the table") $ map (++) (words "on over under across")

    map (map (*2)) [[1..5], [6..10], [11..15]]


Implement `map` ourselves:

> map' :: (a -> b) -> [a] -> [b]
> map' = undefined


`map` generalizes a particular pattern often seen in recursive functions that
process lists, where each item in the input list is passed through some
computation to create an output list of the same size as the input. 

As a HOF, `map` allows us to reuse the pattern without performing explicit
recursion, only having to specify the computation involved in processing each
individual item.


Filter
------

`filter` only keeps values in a list that pass a provided predicate:

    filter :: (a -> Bool) -> [a] -> [a]


Try out:

  filter even [1..10]

  filter (\(a,b,c) -> a^2+b^2 == c^2) $
         [(a,b,c) | a <- [1..10], b <- [a..10], c <- [b..10]]

  filter (\s -> reverse s == s) $ 
         words "madam I refer to adam"

  map (\w -> (w,length w)) $ 
      filter (\s -> reverse s == s) $ 
      words "madam I refer to adam"
  

Implement `filter` ourselves:

> filter' :: (a -> Bool) -> [a] -> [a]
> filter' = undefined


`filter` generalizes yet another pattern seen in recursive list-processing
functions --- one that takes an input list and extract a subset of the elements
based on a test predicate. Again, `filter` lets us reuse the pattern by just
specifying the predicate as an argument function.


Fold
----

"fold" is the name of a class of HOFs that generalize recursion over lists (and
many other data types, as we'll discover). The pattern represented by fold is so
general, in fact, that it can be used to implement *every primitive recursive
function* on lists. 

To see the pattern in action before we abstract it, implement the following:

> sum' :: (Num a) => [a] -> a
> sum' = undefined
>
> and' :: [Bool] -> Bool
> and' = undefined
> 
> concat' :: [[a]] -> [a]
> concat' = undefined


Each of the above recursive functions has some type `[a] -> b`, and is built
around two essential components:

  1. a value used for the base case (the empty list)

  2. a function that combines a value from the list and the result of a
     recursive call, and whose result is the return value of the recursive function

I.e., to express a recursive list function of this type, we need:

  1. a base case return value of type `b`

  2. a combining function with type `a -> b -> b`, where `a` is the input list
     element type, and `b` is the return type of both the recursive and combining functions

Let's design a HOF that encapsulates this notion of primitive list recursion:

> recur :: (a -> b -> b) -> b -> [a] -> b
> recur = undefined


This is our first fold --- specifically, the "right fold".


-- Right fold

The right fold generalizes primitive recursion over lists.

> foldr :: (a -> b -> b) -> b -> [a] -> b
> foldr _ v [] = v
> foldr f v (x:xs) = f x $ foldr f v xs

Note: foldr is actually defined for the "Foldable" type class --- the list is an
instance of Foldable. We'll see how this works later!

Intuitively, foldr "replaces" each cons operator with the combining function,
and the empty list with the base-case value.

E.g., trace out the call `foldr (+) 0 [1..5]`:

    foldr (+) 0 (1 : (2 : (3 : (4 : (5 : [])))))

    = ?

Note that foldr is inherently right-associative.

Let's define some simple recursive functions in terms of foldr:

> sum'' :: (Num a) => [a] -> a
> sum'' = undefined
> 
> product'' :: (Num a) => [a] -> a
> product'' = undefined
>
> and'' :: [Bool] -> Bool
> and'' = undefined
> 
> or'' :: [Bool] -> Bool
> or'' = undefined
>
> concat'' :: [[a]] -> [a]
> concat'' = undefined
>
> stringify' :: (Show a) => [a] -> String
> stringify' = undefined
>
> (+++) :: [a] -> [a] -> [a]
> l1 +++ l2 = undefined
>
> length' :: [a] -> Int
> length' = undefined

And higher order functions:

> map'' :: (a -> b) -> [a] -> [b]
> map'' f = undefined
>
> filter'' :: (a -> Bool) -> [a] -> [a]
> filter'' f = undefined


Sometimes it makes sense to perform a right fold where the base-case value is
just the last value in the list. Let's implement this version:

> foldr1 :: (a -> a -> a) -> [a] -> a
> foldr1 = undefined


This allows us to easily "reduce" a list using a combining function. E.g.,

> sum''' = undefined
>
> product''' = undefined


-- Left fold

Because foldr is right-associative, it may produce unexpected results with
combiner functions that are left-associative. E.g., predict and evaluate the
results of the following:

    foldr1 (-) [10, 5, 3, 2]

    foldr1 (/) [200, 10, 5, 2]

The left-associative version of fold is called the left fold (foldl). Like
foldr, foldl takes a combiner function, a value, and a list as arguments. Unlike
foldr, foldl uses the provided value as an initial argument to the combiner
function, and accumulates the result through its recursive calls.

Here are recursive functions written in the left associative, accumulator style:

> sumL :: (Num a) => a -> [a] -> a
> sumL v [] = v
> sumL v (x:xs) = undefined
>
> andL :: Bool -> [Bool] -> Bool
> andL v [] = v
> andL v (x:xs) = undefined
> 
> concatL :: [a] -> [[a]] -> [a]
> concatL v [] = v
> concatL v (x:xs) = undefined


Let's extract the pattern and define `foldl`:

> foldl :: (b -> a -> b) -> b -> [a] -> b
> foldl f v [] = undefined
> foldl f v (x:xs) = undefined


E.g. trace out the call `foldl (+) 0 [1..5]`:

    foldl (+) 0 (1 : (2 : (3 : (4 : (5 : [])))))

  = ?

---

Note that each level of recursion in foldl adds another application of the
combiner function to the "outside" of the accumulated result. Because of lazy
evaluation, this means we potentially build up an immense "thunk" to be
evaluated later on. E.g.,

    foldl f v [1..N]

    = foldl f (f v 1) [2..N]

    = foldl f (f (f v 1) 2) [3..N]

    = foldl f (f (f (f v 1) 2) 3) [4..N]

    = (f (f ... (f (f (f (f v 1) 2) 3) 4) ...) N)

When the accumulated result needs to be evaluated all at once, this can cause
problems. E.g., try:

    foldl (+) 0 [1..10^8]

The stack overflow is caused by space needed to fully evaluate the thunk (not
for the recursive calls!). This is entirely due to lazy evaluation. 

We can force Haskell to be stricter by using `seq`, which has type:

    seq :: a -> b -> b

`seq` takes two arguments and forces strict evaluation of its first argument
before evaluating the second argument (and returning a result). 

Technically, `seq` only evaluates its first argument to "weak head normal form"
(WHNF), which guarantees that if the outermost part of the argument expression
is a function application, it will be evaluated until that is no longer the
case. Note that a list constructor or other value constructor does not count as
a function application.

We can use `seq` to write a strict version of `foldl` like this:

> foldl' :: (b -> a -> b) -> b -> [a] -> b
> foldl' _ v [] = v
> foldl' f v (x:xs) = let e = f v x in seq e $ foldl' f e xs

With `seq`'s help, `foldl' (+) 0 [1..5]` has the following expansion --- note
the lack of an accumulated thunk:

    foldl' (+) 0 (1 : (2 : (3 : (4 : (5 : [])))))

  = foldl' (+) 1 (2 : (3 : (4 : (5 : []))))

  = foldl' (+) 3 (3 : (4 : (5 : [])))

  = foldl' (+) 6 (4 : (5 : []))

  = foldl' (+) 10 (5 : [])

  = foldl' (+) 15 []

  = 15

---

We can define a version of foldl where the initial argument is simply the first
value in the provided list:

> foldl1 :: (a -> a -> a) -> [a] -> a
> foldl1 f (x:xs) = foldl f x xs


And left-associative operations now work as expected:

    foldl1 (-) [10, 5, 3, 2]

    foldl1 (/) [200, 10, 5, 2]


Additionally, the accumulator pattern built in to foldl allows us to implement
functions like reverse more efficiently:

> reverse' :: [a] -> [a]
> reverse' = undefined


-- On infinite lists

Which folds (if any) work with infinite lists?

Try:

    take 10 $ foldr (:) [] [1..]

    foldr (||) False $ map even [1..]

    foldl (||) False $ map even [1..]

---

Why?

- foldr's combining function is applied to each element *before* the recursive
  call. This allows the combining function to "short circuit" early --- i.e.,
  to not perform the recursion if it isn't necessary.

- foldl builds up an accumulated value from the inside out. This means the first
  computation needed to obtain the result isn't known until all the recursive
  calls have been performed. 
