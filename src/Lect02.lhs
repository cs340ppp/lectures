% CS 340: Programming Paradigms and Patterns
% Lect 02 - Types and Type Classes
% Michael Lee

> module Lect02 where
> import Data.Char 
> import Data.List

Types and Type Classes
======================

Agenda:
  - Types
  - Basic Types
  - Function types
  - Function application
  - Functions of multiple arguments
  - "Operators"
  - Polymorphic functions
  - Type Classes
  - Class constraints


Types
-----

A *type* defines a **collection of values**

  - `e :: T` means that expression `e` (when evaluated) has type `T`

  - all type names start with a capital letter

Haskell has a small number of builtin types, and the "Prelude" module (which is
imported by default into all Haskell source files) defines other standard types and associated functions.


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

What are the types of the following functions?

    not
    isDigit
    toUpper
    ord
    chr


Function application
--------------------

Function application simply requires placing a space between a function name and
its argument(s), e.g.,

    not True

    isDigit '9'


Functions of multiple arguments
-------------------------------

How about functions of multiple arguments?

E.g., interpret the following functions that map from a `Bool` and a `Char` to an `Int`:

    foo1 :: (Bool, Char) -> Int

    foo2 :: Bool -> (Char -> Int)

    foo3 :: Bool -> Char -> Int


Functions of multiple arguments in Haskell are "curried". 

  - What does this mean?
  - What does this say about the associativity of `->`?
  - What does this say about the associativey of function application?
  - What happens if we "partially apply" a function of multiple arguments?


Aside: what about:

    foo4 :: (Bool -> Char) -> Int


"Operators"
-----------

Operators are just functions whose names start with non-letters, and are used
(by default) in infix form (e.g., `13 + 25`)

  - You can ask for information about an operator's type at GHCi using `:i`

    - Also includes information about the precedence and associativity

    - Note: function application has the highest precedence!

  - Check out some operators:

        +
        *
        ^
        **
        &&
        ==
        /=

  - Operators can be used in prefix form if we put them in parentheses (try it!)

  - Functions can be used in infix form (try it with `mod` and `gcd`)


Polymorphic functions
---------------------

Some functions don't care about the specific types of some args/return values. 

  - Their types change (morph) based on the given types of their arguments
  
  - We call these *polymorphic functions*
  
  - We use *type variables* in their type declarations


Check out these polymorphic functions? Can you guess what they do?
  
      id
      const
      fst
      snd
      .
      flip
  

The type declaration of a polymorphic function can give a lot of information about what the function does! (Why?)


Type Classes (aka Classes)
--------------------------

Just as a type is a collection of related values, a type *class* is a collection of related types. 

  - A type class defines functions (known as "methods") supported by all 
    instances (i.e., types) belonging to the class

  - A type can be an instance of multiple classes

  - Classes can inherit from other classes

  - You can get information about a class at GHCi using `:i`

      - This will list the class's methods and instances of the class

Check out these classes and their methods:

    Eq
    Ord
    Num
    Enum
    Integral
    Bounded
    Show


Class constraints
-----------------

Polymorphic function type declarations can include *class constraints* for type variables. The constrained type variables must be instances of specific classes.

Inspect and explain the type declarations for:

    ^
    exp
    /=
    <
    fromIntegral
    read
    sort
    lookup
