% CS 340: Programming Paradigms and Patterns
% Lect 03 - Functions
% Michael Lee

\begin{code}
module Lect03 where
import Data.Char
\end{code}

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

Functions are defined with one or more equations. You should always include a type signature declaration alongside a function definition.

E.g., define the following functions:
  - nand (Boolean not-and)
  - distance (Euclidean distance between two points)


\begin{code}
nand :: Bool -> Bool -> Bool
nand x y = undefined

distance :: (Floating a) => (a, a) -> (a, a) -> a
distance p1 p2 = undefined
\end{code}


-- Pattern matching

Instead of using a variable in a function definition, we can use a *pattern* to match against the parameter value.

E.g., define `not` using pattern matching:

\begin{code}
not' :: Bool -> Bool
not' = undefined
\end{code}


Patterns are matched top down. A variable can be used as a "catch-all" pattern.

E.g., define `fib` (to return the nth Fibonacci number ) using pattern matching:

\begin{code}
fib :: Integer -> Integer
fib = undefined
\end{code}


Sometimes we don't care about the value of a parameter. We use `_` as the matching variable name to indicate this.

E.g., define `nand` again using pattern matching:

\begin{code}
nand' :: Bool -> Bool -> Bool
nand' = undefined
\end{code}


Patterns can also be used to "deconstruct" values. 

E.g., define `fst` and `snd` using pattern matching:

\begin{code}
fst' :: (a,b) -> a
fst' = undefined

snd' :: (a,b) -> b
snd' = undefined
\end{code}


E.g., redefine `distance` using pattern matching:

\begin{code}
distance' :: (Floating a) => (a, a) -> (a, a) -> a
distance' = undefined
\end{code}


E.g., define the `mapTup` function using pattern matching:

\begin{code}
mapTup :: (a -> b) -> (a, a) -> (b, b)
mapTup = undefined
\end{code}


As-patterns can be used to bind a variable to a sub-pattern.

E.g., implement the (very contrived) function `foo`:

\begin{code}
foo :: (a, (b, c)) -> ((a, (b, c)), (b, c), (a, b, c))
foo = undefined
\end{code}


-- Guards

Boolean "guards" can be used to select between multiple right-hand-sides in a single function equation (`otherwise` designates the default).

E.g., redefine `fib` using guards. Is it any clearer?

\begin{code}
fib' :: Integer -> Integer
fib' n = undefined
\end{code}


E.g., define `letterGrade`, which converts a numeric grade to a letter grade:

\begin{code}
letterGrade :: (Ord a, Num a) => a -> Char
letterGrade n = undefined
\end{code}


-- `where` clause

A `where` clause lets us create a local binding for a var or function.

E.g., rewrite `c2h` using a `where` clause:

\begin{code}
c2f :: (Floating a) => a -> a
c2f c = c * 9 / 5 + 32

c2h :: (Floating a, Ord a) => a -> String
c2h c | c2f c >= 100 = "hot"
      | c2f c >= 70  = "comfortable"
      | c2f c >= 50  = "cool"
      | otherwise    = "cold"
\end{code}


Some useful language constructs
-------------------------------

Note: all the constructs in this section define *expressions* --- i.e., each evaluates to a value (which must have a consistent, static type). They are not statements!


-- `if-then-else` expressions

Syntax:

    if e1 then e2 else e3


What's wrong with:

    if n < 0 then True else "False"


E.g., define `closer` which returns the point closest to a source point:

\begin{code}
closer :: (Floating a, Ord a) => (a, a) -> (a, a) -> (a, a) -> (a, a)
closer src dst1 dst2 = undefined
\end{code}


-- `case` expressions

`case` expressions are general pattern-matching forms.

Syntax:

    case exp of pat_1 -> e_1
                pat_2 -> e_2
                ...
                pat_n -> e_n

An `if-then-else` expression is just a special form of `case`:

    if e1 then e2 else e3 === case e1 of True  -> e2
                                         False -> e3

All result expressions must have the same type!

E.g., define `quadrantNames` which returns the name of a quadrant:

\begin{code}
quadrant :: (Int, Int) -> Int
quadrant (x, y) | x > 0 && y > 0 = 1
                | x < 0 && y > 0 = 2
                | x < 0 && y < 0 = 3
                | x > 0 && y < 0 = 4
                | otherwise      = 0

quadrantNames :: (Int, Int) -> String
quadrantNames (x, y) = undefined
\end{code}

-- `let-in` expressions

`let` creates local bindings (for vars/fns) for the expression following `in`. These bindings can also perform pattern matching!

Syntax:

    let pat_1 = e_1
        pat_2 = e_2
        ...
        pat_n = e_n
    in e

E.g., define `quadRoots` which returns the roots of a quadratic equation:

\begin{code}
quadRoots :: Double -> Double -> Double -> (Double, Double)
quadRoots a b c = undefined
\end{code}
