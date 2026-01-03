---
title: Functions
author: "Michael Lee <lee@iit.edu>"
---

# Agenda

- Defining functions
  - Pattern matching
  - Guards
  - `where` clause
- Some useful language constructs
  - `if-else` expressions
  - `case` expressions
  - `let-in` expressions

---

# Defining Functions

Functions are defined with one or more equations. You should always include a
type signature declaration alongside a function definition.

```haskell
nand :: Bool -> Bool -> Bool
nand x y = not (x && y)

distance :: (Floating a) => (a, a) -> (a, a) -> a
distance (x1, y1) (x2, y2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)
```

**Key principle**: Type signatures document intent and help catch errors early.

---

# Pattern Matching

Instead of using a variable in a function definition, we can use a *pattern* to
match against the parameter value.

```haskell
not :: Bool -> Bool
not True  = False
not False = True
```

Patterns are matched top down, and you can use a variable as a "catch-all"
pattern.

---

## Pattern Matching: Numeric Patterns

```haskell
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
```

The first matching pattern is used, so order matters!

---

## Pattern Matching: Wildcards

Sometimes we don't care about the value of a parameter. We use `_` as the
matching variable name to indicate this.

```haskell
nand :: Bool -> Bool -> Bool
nand False _ = True
nand _ False = True
nand _ _     = False
```

The wildcard `_` says "I don't need this value."

---

## Pattern Matching: Deconstructing Tuples

Patterns can also be used to "deconstruct" values.

```haskell
fst :: (a, b) -> a
fst (x, _) = x

snd :: (a, b) -> b
snd (_, y) = y
```

We can extract tuple components right in the pattern!

---

## Pattern Matching: Working with Tuples

```haskell
distance :: (Floating a) => (a, a) -> (a, a) -> a
distance (x1, y1) (x2, y2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)
```

Compare to the original `distance` — pattern matching makes the code cleaner.

---

## Pattern Matching: Completeness

Pattern matching should be *complete* (or *exhaustive*); i.e., covering all
possible input values.

```haskell
classifyPoint :: (Int, Int) -> String
classifyPoint (0, 0) = "origin"
classifyPoint (_, 0) = "x-axis"
classifyPoint (0, _) = "y-axis"
```

GHC warns about incomplete patterns:

```
warning: [-Wincomplete-patterns]
    Pattern match(es) are non-exhaustive
    In an equation for 'classifyPoint':
        Patterns of type '(Int, Int)' not matched: (p, p1) where ...
```

*At runtime*, calling `classifyPoint (3, 4)` causes an exception:

```
*** Exception: Non-exhaustive patterns in function classifyPoint
```

---

## Pattern Matching: Making It Complete

Add a catch-all pattern to handle remaining cases:

```haskell
classifyPoint :: (Int, Int) -> String
classifyPoint (0, 0) = "origin"
classifyPoint (_, 0) = "x-axis"
classifyPoint (0, _) = "y-axis"
classifyPoint _      = "off-axis"
```

*Best practice*: Endeavor to make your pattern matches complete. The compiler
warning is your friend!

---

## Aside: Partial Functions

Some functions are *partial* by design -- i.e., they aren't defined for all
inputs. We should document this and can fail with an explicit error message:

```haskell
-- `get3 i t` returns the `i`-th (0 <= `i` <= 2) element in `t`
get3 :: Int -> (a, a, a) -> a
get3 0 (x, _, _) = x
get3 1 (_, y, _) = y  
get3 2 (_, _, z) = z
get3 _ _         = error "get3: index out of bounds"
```

---

## Pattern Matching: As-Patterns

As-patterns let you bind a variable to a value while also pattern matching on
its substructure(s).

Syntax: `var@pattern`

```haskell
pointWithDistance :: (Floating a) => (a, a) -> ((a, a), a)
pointWithDistance p@(x, y) = (p, sqrt (x^2 + y^2))
```

Here `p` refers to the whole tuple, while `x` and `y` are its components.

---

## Pattern Matching Across Languages

Pattern matching is a powerful feature appearing in many modern languages:

**Python 3.10+:**

```python
match point:
    case (0, 0):
        return "origin"
    case (x, 0):
        return f"x-axis at {x}"
    case (0, y):
        return f"y-axis at {y}"
```

**Rust:**

```rust
match point {
    (0, 0) => println!("origin"),
    (x, 0) => println!("x-axis at {}", x),
    (0, y) => println!("y-axis at {}", y),
    (x, y) => println!("({}, {})", x, y),
}
```

---

# Guards

Boolean "guards" can be used to select between multiple right-hand-sides in a
single function equation.

```haskell
fib :: Integer -> Integer
fib n | n == 0    = 0
      | n == 1    = 1
      | otherwise = fib' (n - 1) + fib' (n - 2)
```

`otherwise` is just a synonym for `True` — it designates the default case.

---

## Guards: Ordered Conditions

Guards are checked top to bottom. Use this to write cleaner conditional logic.

```haskell
letterGrade :: (Ord a, Num a) => a -> Char
letterGrade n | n >= 90   = 'A'
              | n >= 80   = 'B'
              | n >= 70   = 'C'
              | n >= 60   = 'D'
              | otherwise = 'F'
```

Much clearer than nested if-expressions!

---

## `where` Clause

A `where` clause lets us create local bindings for variables or functions.

```haskell
c2f :: (Floating a) => a -> a
c2f c = c * 9 / 5 + 32

-- Without where: repeated computation
c2h :: (Floating a, Ord a) => a -> String
c2h c | c2f c >= 100 = "hot"
      | c2f c >= 70  = "comfortable"
      | c2f c >= 50  = "cool"
      | otherwise    = "cold"
```

Notice we call `c2f c` four times!

---

## `where` Clause: Avoiding Repetition

```haskell
c2h :: (Floating a, Ord a) => a -> String
c2h c | f >= 100   = "hot"
      | f >= 70    = "comfortable"
      | f >= 50    = "cool"
      | otherwise  = "cold"
  where f = c2f c
```

The `where` clause binds `f` locally, making the code cleaner and more
efficient.

---

# Some Useful Language Constructs

- `if-then-else`
- `case`
- `let-in`

**Important**: All these constructs define *expressions* — each evaluates to a
value (which must have a consistent, static type). They are not statements!

This is a key difference from imperative languages.

---

## `if-then-else` Expressions

Syntax:

```
if e1 then e2 else e3
```

Both branches must have the same type!

What's wrong with this?

```haskell
if n < 0 then True else "False"
```

The `then` branch has type `Bool`, but the `else` branch has type `String`.

---

## `if-then-else`: Example

```haskell
closer :: (Floating a, Ord a) => (a, a) -> (a, a) -> (a, a) -> (a, a)
closer src dst1 dst2 = if distance src dst1 <= distance src dst2
                       then dst1
                       else dst2
```

Returns the point closest to the source point.

---

## `case` Expressions

`case` expressions are the general pattern-matching form.

Syntax:

```
case exp of pat_1 -> e_1
            pat_2 -> e_2
            ...
            pat_n -> e_n
```

**Key insight**: Pattern matching in function definitions is actually *syntactic
sugar* for `case` expressions!

---

## `case` Expressions: Relationship to `if`

An `if-then-else` expression is just a special form of `case`:

```haskell
if e1 then e2 else e3 
  === 
case e1 of 
  True  -> e2
  False -> e3
```

All result expressions must have the same type!

---

## `case` Expressions: Example

```haskell
verboseCompare :: Int -> Int -> String
verboseCompare x y = 
  "Comparing " ++ show x ++ " and " ++ show y ++ ": " ++
  case (abs x, abs y) of
    (0, 0) -> "both zero"
    (0, _) -> "first is zero"
    (_, 0) -> "second is zero"
    (a, b) | a > b     -> "first has larger magnitude"
           | a < b     -> "second has larger magnitude"
           | otherwise -> "equal magnitudes"
```

`case` lets us pattern match anywhere in an expression!

---

## `let-in` Expressions

`let` creates local bindings (for vars/fns) for the expression following `in`.
These bindings can also perform pattern matching!

Syntax:

```
let pat_1 = e_1
    pat_2 = e_2
    ...
    pat_n = e_n
in e
```

---

## `let-in`: Example

```haskell
quadRoots :: Double -> Double -> Double -> (Double, Double)
quadRoots a b c = let discriminant = b^2 - 4*a*c
                      sqrtDisc = sqrt discriminant
                      twoA = 2*a
                  in ((-b + sqrtDisc) / twoA, (-b - sqrtDisc) / twoA)
```

`let-in` bindings are expressions, so they can be used anywhere an expression is
expected.

---

## `where` vs. `let-in`

Both create local bindings, but with different scopes:

- **`where`**: Bindings span all guards in a function equation
  - Appears at the end of the definition
  - Good for definitions that apply to multiple guards

- **`let-in`**: Bindings only scope over the expression after `in`
  - Can be used anywhere an expression is expected
  - Good for intermediate computations within an expression

Style preference varies, but both are useful in different contexts!

---

## Summary

**Functions**: Always include type signatures!

**Pattern Matching**: Match against structure and values

- Works on literals, tuples, and more
- Patterns checked top to bottom
- Use `_` for values you don't need
- As-patterns (`var@pattern`) bind both whole and parts

**Guards**: Boolean conditions for selecting equations

**Local Bindings**: `where` and `let-in` for local definitions

**Expressions**: `if-then-else`, `case`, and `let-in` are all expressions that
evaluate to values

These tools give you expressive power to write clean, declarative code!
