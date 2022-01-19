% CS 340: Programming Paradigms and Patterns
% Lect 02 - Types and Type Classes
% Michael Lee

> module Lect02 where
> import Data.Char

Types and Type Classes
======================

Agenda:
  - Types
  - Basic Types
  - Function types
  - Function application
  - "Operators"
  - Polymorphic functions
  - Type Classes


Types
-----

A *type* defines a collection of values.

  - `e :: T` means that expression `e` (when evaluated) has type `T`

  - all type names start with a capital letter

Haskell has a small number of builtin types, and the "Prelude" module (which is
imported by default into all Haskell source files) defines other standard types.


Basic types
-----------

  - Bool    - True/False
  - Char    - Unicode character
  - Int     - 64 bit signed integer
  - Integer - arbitrary-precision integer
  - Float   - 32-bit IEEE single-precision floating point number
  - Double  - 64-bit IEEE double-precision floating point number
  - Tuple   - finite (i.e., of a given arity) sequence of zero or more types

(At GHCi, we can use `:t` to ask for the type of any expression.)

What are the types of the following?

    True
    'a'
    5
    1.5
    ('b', False, 'c')
    ()
    (True)
    (1, 2, 3, True)


Function types
--------------

A function is a mapping from one type (the domain) to another type (the range).

For a function that maps type T1 to type T2, we specify its type as `T1 -> T2`

Some functions:

    not  :: Bool -> Bool

    isDigit :: Char -> Bool


A function of multiple arguments can be implemented in different ways:

  1. A function that takes a tuple of the requisite types, e.g.,

         foo :: (Bool, Char) -> Int

  2. Functions that return other functions, e.g.,

         foo :: Bool -> (Char -> Int)

     We interpret this type as a function which takes a `Bool` and returns
     another function which takes a `Char` and returns an `Int`.

     Here's a function of three `Int` arguments:

         bar :: Int -> (Int -> (Int -> Int))

     We call functions that take arguments one at a time like this "curried"
     functions. This is the default in Haskell, so the `->` in type declarations
     associates to the right, which means we can just write:

         bar :: Int -> Int -> Int -> Int


Function application
--------------------

Function application simply requires placing a space between a function name and
its argument(s), e.g.,

    not True

    isDigit '9'


Function application associates left-to-right, so 

    foo 5 True 'a'

is equivalent to:

    (((foo 5) True) 'a')

Note that this is in keeping with function currying --- each function
application results in another function, that is then applied to an additional
argument to obtain another function ...

We will be making use of *partial function application* quite a bit!


"Operators"
-----------

Operators are just functions whose names start with non-letters, and are used
in infix form (e.g., `13 + 25`)

  - Operators can be used in prefix form if we place them in parentheses

        (&&) True False

        (+) 13 25

        (^) 2 10

  - Functions can be used in infix form if we place them in backticks (``)

        20 `mod` 3

        36 `gcd` 27


Polymorphic functions
---------------------

Some functions are type-agnostic; i.e., they don't care about the types of some
of their arguments. E.g., consider a function that takes a tuple and returns 
the first element.

Such functions are still well-typed, but their types change (morph) according to
their specific argument types. We call these functional *polymorphic functions*,
and their type declarations contain *type variables*.

E.g., a function which takes a two-tuple and returns the first element has type:

    fst :: (a, b) -> a


Since an unqualified type variable says nothing about its actual type, you can't
do much with the value of the corresponding argument.

But the type of a polymorphic function can actually be quite helpful in
determining what it does!

e.g., what do `snd`, `id`, `const`, do, based on their types?

e.g., try to decipher the types of `.` and `until`


Type Classes (aka Classes)
--------------------------

Just as a type is a collection of related values, a type *class* is a collection
of related types. A class defines functions (known as methods) that are
supported by all instances (i.e., types) of that class

Some common classes, their methods, and instances:

    class Eq a where
      (==) :: a -> a -> Bool
      (/=) :: a -> a -> Bool

      instances: Int, Integer, Float, Double, Char, Bool


    class Ord a where
      compare :: a -> a -> Ordering
      (<) :: a -> a -> Bool
      (<=) :: a -> a -> Bool
      (>) :: a -> a -> Bool
      (>=) :: a -> a -> Bool
      max :: a -> a -> a
      min :: a -> a -> a

      instances: Int, Integer, Float, Double, Char, Bool


    class Num a where
      (+) :: a -> a -> a
      (-) :: a -> a -> a
      (*) :: a -> a -> a
      negate :: a -> a
      abs :: a -> a
      signum :: a -> a
      fromInteger :: Integer -> a

      instances: Int, Integer, Float, Double


Polymorphic function type declarations can also include *class constraints*, 
which indicate that constrained type variables must be instances of specific 
classes.

E.g., the following type declaration says that type variable `a` must be an
instance of the `Num` class:

    subtract :: Num a => a -> a -> a


Inspect the types of `^`, `show`, `read`, `length`. (At GHCi, you can use `:i` 
to get more information about types, classes, methods, and class instances.)
