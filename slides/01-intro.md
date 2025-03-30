---
title: Introduction to Haskell
author: Michael lee <lee@iit.edu>
---

# Agenda

- Required tools
- Lecture repository organization
- Notable (& maybe surprising) language features
- Indentation and layout rules

---

# Required tools

- Git: a version control system

- GitHub: a Git hosting service

- GHCUp: an installer for Haskell development tools

- GHC: the Glasgow Haskell Compiler

  - GHCi: GHC's interactive environment (REPL)

- Stack: a Haskell build tool

---

# Lecture repository organization

- `lectures/slides`: [Markdown][markdown-guide] formatted slides
  - I use [presenterm](https://mfontanini.github.io/presenterm/) to present them
    in class
- `lectures/src`: Haskell source files (numbers correspond to slides)
  - e.g., "src/Lect01.hs" --> "slides/01-intro.md"

[markdown-guide]: https://www.markdownguide.org/

---

# Notable language features

1. Conciseness
2. Strong, static typing
3. Type inference
4. Sophisticated type system
5. Purely functional
6. Lazy evaluation

---

# Notable language features

## 1. Conciseness

Small language with few keywords:

- `case`, `class`, `data`, `deriving`, `do`, `else`, `if`, `import`, `in`,
  `infix`, `infixl`, `infixr`, `instance`, `let`, `module`, `newtype`, `of`,
  `then`, `type`, `where`

Declarative vs. Imperative style!

<!-- pause -->

```haskell
quicksort []     = []
quicksort (x:xs) = quicksort lesser ++ [x] ++ quicksort larger
    where lesser = filter (< x)  xs
          larger = filter (>= x) xs
```

- aided by *pattern-matching*

---

# Notable language features

## 2. Strong, static typing

- Every expression or variable has a type associated with it that cannot change

  - Rigidly enforced by the compiler

- Haskel programs are *type-safe*

  - There will *never* be run-time type related errors (in compiled programs)

<!-- pause -->

E.g., function definition and call:

```haskell
foo :: Int -> Int
foo x = 2 * x

foo (1 :: Integer)
```

<!-- pause -->

Compilation error:

```
• Couldn't match expected type ‘Int’ with actual type ‘Integer’
• In the first argument of ‘foo’, namely ‘(1 :: Integer)’
  In the expression: foo (1 :: Integer)
```

---

# Notable language features

## 3. Type inference

The compiler can automatically figure out the types of most things on its own

- Best of both static & dynamic typing worlds: we don't have to spell out type
  declarations, but we still get typechecking

<!-- pause -->

E.g., undeclared variables used in a boolean expression:

```haskell
let x = "True"
    y = False
in x && y
```

<!-- pause -->

Compilation error:

```
• Couldn't match type ‘[Char]’ with ‘Bool’
  Expected: Bool
    Actual: String
```

---

# Notable language features

## 4. Sophisticated type system

A very rich, expressive system for defining and working with complex, abstract
data types.

- Algebraic data types and Type classes

- Parametric polymorphism (similar to generics)

- Ad-hoc polymorphism

<!-- pause -->

E.g., a parametric data type, type class, and conforming instance:

```haskell
data Maybe a = Just a | Nothing

class Functor f where
  fmap :: (a -> b) -> f a -> f b

instance Functor Maybe where
  fmap :: (a -> b) -> Maybe a -> Maybe b
  fmap _ Nothing  = Nothing
  fmap f (Just x) = Just (f x)
```

---

# Notable language features

## 5. Purely functional

Once variables are bound to values they are *immutable*

<!-- pause -->

All functions are *pure* -- they always return the *same output for a given set
of inputs*

- no *side effects*; i.e., no changes to external/global state

<!-- pause -->

Rsults in *referential transparency*; i.e., expressions can be replaced with
values *at any time* without affecting behavior

<!-- pause -->

E.g., in what order should the function calls be evaluated? (it doesn't
matter!!!)

```haskell
foo (bar 340) (bat (bam 42) (baz 43))
```

---

# Notable language features

## 6. Lazy evaluation

Expressions (e.g., function calls) are not evaluated until their results are
actually needed.

Enables *infinite data structures* and short-circuiting behavior in functions
(which require control-flow structures in non-lazy languages)!

<!-- pause -->

```haskell
-- builds the list 1 : 2 : 3 : 4 : 5 : ...
inflist :: [Int]
inflist = gen 1
  where gen x = x : gen (x+1) -- infinite recursion

take 3 inflist --> 1 : 2 : 3 : []
```
