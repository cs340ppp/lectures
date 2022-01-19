% CS 340: Programming Paradigms and Patterns
% Lect 03 - Functions
% Michael Lee

> module Lect03 where

Functions
=========

Agenda:
  - Defining functions
    - Pattern matching
    - Guards
    - `where` clause
  - Some useful language constructs
    - `if-else` expressions
    - `case` expressions
    - `let-in` expressions


Defining Functions
------------------

A named, top-level function definition starts with its name, is followed by its
parameter list (separated by spaces), then `=`, then an expression which will 
be evaluated to determine its result.

By convention, we always include type declarations for functions.

>
> nand a b = not (a && b)
>
>
> 
> discriminant a b c = b^2 - 4*a*c
>
>
> 
> c2f c = c * 9/5 + 32
 

-- Pattern matching

We can provide multiple alternative expressions to be evaluated when a function
is called, which are differentiated based on *patterns* that are matched against
the parameter values. Patterns are matched in order (top to bottom); only the
first pattern to match has its corresponding expression evaluated.

> not' :: Bool -> Bool
> not' = undefined


A catch-all pattern, where a variable is specified instead of a data value, can 
be used to match parameters not specified in earlier patterns.

> fib :: Integer -> Integer
> fib = undefined


We can also use the wildcard pattern `_` to match on one or more values we don't
care about.

> nand' :: Bool -> Bool -> Bool
> nand' = undefined


Patterns can also be used to "deconstruct" values. E.g., for tuples:

> fst' :: (a,b) -> a
> fst' = undefined
>
> 
> distance :: (Floating a, Eq a) => (a,a) -> (a,a) -> a
> distance p1 p2 = sqrt ((fst p1 - fst p2)^2 + (snd p1 - snd p2)^2)
>
>
> mapTup :: (a -> b) -> (a,a) -> (b,b)
> mapTup f tup = (f (fst tup), f (snd tup))


-- Guards

Boolean expressions can be used to provide more granular *guards* for separate
equations in a function definition. The `otherwise` keyword (which is just 
`True` in disguise) can be used to provide a catch-all equation.

> fib' :: Integer -> Integer
> fib' n = undefined
>
>
> c2h :: (Floating a, Ord a) => a -> String
> c2h c = undefined
>
>
> quadRoots :: (Floating a, Ord a) => a -> a -> a -> (a, a)
> quadRoots a b c = undefined


-- `where` clause

A `where` clause lets us introduce new local bindings (vars or functions) in a 
given function definition (which may span multiple guards, but *not* separate 
top-level patterns). Note that we read the `|` symbol as "such that".

> quadRoots' :: (Floating a, Ord a) => a -> a -> a -> (a, a)
> quadRoots' a b c = undefined


Some useful language constructs
-------------------------------

An important note about the following constructs: they are all used to create
*expressions* --- i.e., they evaluate to values (which must have a consistent,
static type regardless of the evaluation path). They are not statements!


-- `if-else` expressions

The classic conditional (but both paths must exist, and must also evaluate to 
the same type!)

> -- try:
> -- oneOrOther n = if n < 0 then True else "False"
>
>
> fib'' :: Integer -> Integer
> fib'' n = undefined


-- `case` expressions

`case` expressions allow us to perform pattern matching --- just as we can 
across top-level function definitions --- on an arbitrary expression. Patterns
can also be followed by guards!

> greet :: String -> String
> greet name = undefined


-- `let-in` expressions

Similar to a `where` clause, the `let` keyword allows us to create new 
bindings, but only usable within the expression following the `in` keyword.
The entire `let-in` construct is also an *expression* --- it evaluates to the
value of the expression following `in`.

> quadRoots'' :: (Floating a, Ord a) => a -> a -> a -> (a, a)
> quadRoots'' a b c = undefined
>
>
> dist2h :: (Floating a, Ord a, Show a) => (a,a) -> String
> dist2h p = undefined
