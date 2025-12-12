---
title: "Types"
sub_title: "CS 340: Programming Patterns and Paradigms"
author: "Michael Lee <lee@iit.edu>"
---

# Agenda

- Types and Type Systems
- Type Annotations and Type Inference
- Function Types
- Polymorphic Types and Parametric Polymorphism
- Type Classes and Ad Hoc Polymorphism
- Kinds and Higher-Order Types

<!--
speaker_note: |
  You'll need to import Data.char and Data.List for the examples given.
-->

---

# What's a *Type*?

<!-- pause -->

A *type* classifies a set of *values* (aka *terms*), and *expressions* that
evaluate to them.

<!-- pause -->

- Simple: `Boolean`: `True` & `False`; `Int`: `0`, `1`, `2`, ...

- Compound: tuples, lists, records

- Functions are values too!

---

# What's a "Type System"?

<!-- pause -->

A *type system* assigns static types to program constructs, and *enforces rules
on their use*.

<!-- pause -->

Haskell's type system guarantees that well-typed programs *never produce type
errors at runtime* (type-safety).

<!-- pause -->

- To ensure this, the compiler is strict -- we must learn its rules!

---

# Some Types

<!-- incremental_lists: true -->

- `Bool`: `True`/`False`
- `Char`: Unicode character
- `String`: list of `Char`
- `Int`: 64-bit signed integer
- `Integer`: arbitrary-precision integer
- `Double`: 64-bit double-precision floating point number

Type names are *always capitalized*

---

# Type Annotations

We can explicitly attach a type to an expression in Haskell, and the compiler
will check it for us (try messing some up!)

```haskell
True :: Bool
42 :: Int
2 ^ 100 :: Integer
"hello" !! 0 :: Char
sqrt 2 :: Double
```

---

# Type Inference

We rarely type-annotate expressions manually. Haskell is capable of *inferring*
types for us.

<!-- pause -->

At GHCi, `:t` tells us the inferred type of an expression.

- Try it with the earlier expressions -- what types do you get?

<!-- pause -->

Haskell's type inference will always infer (and enforce) the *most general
possible type* for an expression.

---

# Function types

<!-- pause -->

How would you describe the type of a function like `sqrt`?

<!-- pause -->

In Haskell, we use an arrow (`->`) to separate the argument type and return type
in a function type. Intuitively, `->` means "maps to" (as from domain to
co-domain).

<!-- pause -->

Check out the types of these functions (using `:t`):

- `not`, `isDigit`, `toUpper`, `ord`, `chr`

---

# Functions of Multiple Arguments

<!-- pause -->

One way to describe a function of multiple arguments is like this:

`add :: (Int, Int) -> Int`

- i.e., `add` is a function that maps two `Int`s to a single `Int`

<!-- pause -->

But it is much more common to write them like this:

`add :: Int -> (Int -> Int)`

<!-- pause -->

- i.e., `add` is a function that maps an `Int` to another function that maps an
  `Int` to an `Int`

- `->` is right-associative, so `Int -> (Int -> Int)` = `Int -> Int -> Int`

---

# "Currying"

This one-argument-at-a-time nature of functions is known as *currying*.

- Named after mathematician Haskell Curry

<!-- pause -->

It may seem odd, but is actually incredibly useful and powerful!

<!-- pause -->

Check out the types of the functions `&&` and `||`. Note that "operators" are
just functions whose names start with symbols.

---

# Kinds

Not all types classify terms directly. Those that do (the ones discussed so
far), are called *concrete types*.

<!-- pause -->

- We say concrete types are *inhabited* by terms

<!-- pause -->

- We classify concrete types with the *kind* `Type`

<!-- pause -->

I.e, *term*s are grouped by *type*, and *type*s are grouped by *kind*

<!-- pause -->

- E.g., `True` & `False` ∈ `Bool`, and `Bool`, `Char`, etc. ∈ `Type`

---

# Higher-Order Types

*Higher-order types* are constructed from other types, and are not on their own
inhabited by terms.

<!-- pause -->

E.g., `Set` defines an unordered collection of elements (of homogeneous type)

<!-- incremental_lists: true -->

- To specify a concrete set, we need to specify the element's type

  - `Set Int` is a set of integers, `Set String` is a set of strings

- Type `Set` has kind `Type -> Type`

  - `Set` is a *type constructor*: it takes a concrete type and returns a
    concrete type

---

# Some Higher-Order Types

You can inspect the kind of a type in GHCi with `:k`

Check out the kinds of these types:

- `[]`, `Maybe`, `(,)`, `(,,)`

For historical reasons, GHCi prints `*` for `Type`

- E.g., `* -> *` means `Type -> Type`

---

# Polymorphic Types

Often, we want expressions that handle terms belonging to *arbitrary types*. We
call these expressions *polymorphic*.

<!-- pause -->

We use *type variables* as placeholders for arbitrary types.

<!-- pause -->

E.g., consider the following function type (called "identity"):

```haskell
id :: a -> a
```

<!-- pause -->

- `id` is a function which takes a term of arbitrary type `a` and returns a term
  of the same type

---

# Polymorphic Types: A Closer Look

Under the hood, `id` actually has the type:

```haskell
id :: forall (a :: Type). a -> a
```

<!-- incremental_lists: true -->

- `id` has a type parameter `a` (of kind `Type`), which it uses to *instantiate*
  its polymorphic type
- If we substitute `Int` for `a`, (`a ~ Int`), then `id` has the instantiated
  type `Int -> Int`

We call this *Parametric Polymorphism*

---

# Parametric Polymorphism = "Generics"

In other languages, *generics* play the same role as parametric polymorphism.

<!-- pause -->

E.g., in Java:

```java
public class Box<T> {
  private T t;

  public void set(T t) { this.t = t; }
  public T get() { return t; }
}

Box<Integer> integerBox = new Box<Integer>();
```

- `T` is the type parameter for `Box`

---

# Some Polymorphic Functions

Check out the types of these polymorphic functions: `id`, `const`, `fst`,
`repeat`

- Can you guess what they do?

---

# Limits of Parametric Polymorphism

The identity function *cannot do anything other than return its argument*

```haskell
id :: a -> a
```

<!-- pause -->

- *Nothing is known* about type `a` (what terms inhabit it, what operations are
  permitted on it, etc.)
- Parametric polymorphic functions apply a *single abstract implementation*
  regardless of the argument type

---

# Ad Hoc Polymorphism

We might want a polymorphic type to admit arbitrary types *contingent on some
known behavior*.

<!-- pause -->

E.g., Consider the (valid) tests `1 == 1` and `1.0 == 1.00` and `"hi" == "hi"`

<!-- pause -->

- `==` is polymorphic, but only for types that support equality testing

- Each type (e.g., `Int`, `Double`, `String`) must implement `==` in a
  type-specific manner

<!-- pause -->

We need a mechanism that (1) classifies types by behavior, and (2) allows types
to implement that behavior independently.

---

# Type Classes (aka Classes)

A *type class* defines a collection of related functions (known as *methods*).

<!-- pause -->

A type may be declared an *instance* of the type class, if it implements all the
required methods (some may already be provided by the class).

<!-- pause -->

E.g., here's the type class `Eq`:

```haskell
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
```

<!-- pause -->

- `Bool`, `Char`, `Double`, `Int`, `Integer` are all instances of `Eq`

<!-- pause -->

Check out the types of the `==` and `/=` functions in GHCi (using `:t`).

---

# Class Constraints

A *class constraint* is a qualifier that constrains the type parameter(s) found
in a polymorphic type.

<!-- pause -->

E.g., `equal` uses `Eq` as a class constraint so that it can search for a
matching element in a list:

```haskell
elem :: Eq a => a -> [a] -> Bool
```

<!-- pause -->

- Read: "For every type `a` *that is an instance of* `Eq`, `elem` has the type
  &nbsp; `a -> [a] -> Bool`"

<!-- pause -->

- When `==` or `/=` are used in `elem`, the appropriate instance definition is
  used. This is called *ad hoc polymorphism*.

---

# Class Extension

Classes may *inherit* methods from another class in order to extend them.

<!-- pause -->

E.g., here's the type class `Ord`, which extends `Eq`:

```haskell
class Eq a => Ord a where
  (<), (<=), (>=), (>) :: a -> a -> Bool
  max :: a -> a -> a
  min :: a -> a -> a
  compare :: a -> a -> Ordering
```

<!-- pause -->

Any instance of `Ord` must also be an instance of `Eq`

- `Eq` is a superclass of `Ord`, `Ord` is a subclass of `Eq`

---

# A Few More Type Classes

Check out these type classes (with `:i`):

- `Num`, `Enum`, `Integral`, `Bounded`, `Show`

You'll see the class definition itself along with a (sometimes lengthy) list of
its instances.

---

# Summary

- `term`s inhabit `type`s

- `type`s inhabit `kind`s

- `type constructors` are functions on types

- `type variables` enable parametric polymorphism

- `type classes` enable ad-hoc polymorphism

---

# Summary

You can now decipher arbitrarily complex type signatures such as:

```haskell
find :: Foldable t => (a -> Bool) -> t a -> Maybe a

product :: (Foldable t, Num a) => t a -> a

insert :: Ord k => k -> a -> Map k a -> Map k a

state :: MonadState s m => (s -> (a, s)) -> m a
```

<!-- pause -->

We will spend the rest of the semester studying *programs that inhabit these
types* and others!
