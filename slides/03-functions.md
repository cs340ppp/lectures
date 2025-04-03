---
title: Functions
author: Michael Lee <lee@iit.edu>
---

# Functions

Agenda:

- Defining functions
  - Pattern matching
  - Guards
  - `where` clause
- Some useful language constructs
  - `if-else` expressions
  - `case` expressions
  - `let-in` expressions

## Defining Functions

Functions are defined with one or more equations. You should always include a
type signature declaration alongside a function definition.

E.g., define the following functions:

- nand (Boolean not-and)
- distance (Euclidean distance between two points)

```haskell
nand :: Bool -> Bool -> Bool
nand x y = undefined

distance :: (Floating a) => (a, a) -> (a, a) -> a
distance p1 p2 = undefined
```

-- Pattern matching

Instead of using a variable in a function definition, we can use a *pattern* to
match against the parameter value.

E.g., define `not` using pattern matching:

```haskell
not' :: Bool -> Bool
not' = undefined
```

Patterns are matched top down. A variable can be used as a "catch-all" pattern.

E.g., define `fib` (to return the nth Fibonacci number ) using pattern matching:

```haskell
fib :: Integer -> Integer
fib = undefined
```

Sometimes we don't care about the value of a parameter. We use `_` as the
matching variable name to indicate this.

E.g., define `nand` again using pattern matching:

```haskell
nand' :: Bool -> Bool -> Bool
nand' = undefined
```

Patterns can also be used to "deconstruct" values.

E.g., define `fst` and `snd` using pattern matching:

```haskell
fst' :: (a,b) -> a
fst' = undefined

snd' :: (a,b) -> b
snd' = undefined
```

E.g., redefine `distance` using pattern matching:

```haskell
distance' :: (Floating a) => (a, a) -> (a, a) -> a
distance' = undefined
```

E.g., define the `mapTup` function using pattern matching:

```haskell
mapTup :: (a -> b) -> (a, a) -> (b, b)
mapTup = undefined
```

As-patterns can be used to bind a variable to a sub-pattern.

E.g., implement the (very contrived) function `foo`:

```haskell
foo :: (a, (b, c)) -> ((a, (b, c)), (b, c), (a, b, c))
foo = undefined
```

-- Guards

Boolean "guards" can be used to select between multiple right-hand-sides in a
single function equation (`otherwise` designates the default).

E.g., redefine `fib` using guards. Is it any clearer?

```haskell
fib' :: Integer -> Integer
fib' n = undefined
```

E.g., define `letterGrade`, which converts a numeric grade to a letter grade:

```haskell
letterGrade :: (Ord a, Num a) => a -> Char
letterGrade n = undefined
```

-- `where` clause

A `where` clause lets us create a local binding for a var or function.

E.g., rewrite `c2h` using a `where` clause:

```haskell
c2f :: (Floating a) => a -> a
c2f c = c * 9 / 5 + 32

c2h :: (Floating a, Ord a) => a -> String
c2h c | c2f c >= 100 = "hot"
      | c2f c >= 70  = "comfortable"
      | c2f c >= 50  = "cool"
      | otherwise    = "cold"
```

## Some useful language constructs

Note: all the constructs in this section define *expressions* --- i.e., each
evaluates to a value (which must have a consistent, static type). They are not
statements!

-- `if-then-else` expressions

Syntax:

    if e1 then e2 else e3

What's wrong with:

    if n < 0 then True else "False"

E.g., define `closer` which returns the point closest to a source point:

```haskell
closer :: (Floating a, Ord a) => (a, a) -> (a, a) -> (a, a) -> (a, a)
closer src dst1 dst2 = undefined
```

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

```haskell
quadrant :: (Int, Int) -> Int
quadrant (x, y) | x > 0 && y > 0 = 1
                | x < 0 && y > 0 = 2
                | x < 0 && y < 0 = 3
                | x > 0 && y < 0 = 4
                | otherwise      = 0

quadrantNames :: (Int, Int) -> String
quadrantNames (x, y) = undefined
```

-- `let-in` expressions

`let` creates local bindings (for vars/fns) for the expression following `in`.
These bindings can also perform pattern matching!

Syntax:

    let pat_1 = e_1
        pat_2 = e_2
        ...
        pat_n = e_n
    in e

E.g., define `quadRoots` which returns the roots of a quadratic equation:

```haskell
quadRoots :: Double -> Double -> Double -> (Double, Double)
quadRoots a b c = undefined
```
