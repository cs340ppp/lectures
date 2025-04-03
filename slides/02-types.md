---
title: "Types and Type Classes"
sub_title: "CS 340: Programming Patterns and Paradigms"
author: "Michael Lee <lee@iit.edu>"
---

# Agenda

- Types
- Basic Types
- Function types
- Function application
- Functions of multiple arguments
- "Operators"
- Polymorphic functions
- Type Classes
- Class constraints

---

# Types

- What is a *type*?

- How do we indicate the type of an expression in Haskell?

---

# Basic types

- `Bool`: True/False
- `Char`: Unicode character
- `String`: a list of `Char`s
- `Int`: 64 bit signed integer
- `Integer`: arbitrary-precision integer
- `Float`: 32-bit IEEE single-precision floating point number
- `Double`: 64-bit IEEE double-precision floating point number
- `Tuple`: finite sequence of zero or more (fixed types of) values

We can ask for the type of a value in GHCi with the `:t` command -- try it out
with the following values.

```
True
'a'
"hello"
(False, 'b', "hi")
()
(True)
42
```

---

# Function types

- How would we describe a function in terms of types?

- How do we specify function types in Haskell?

What are the types of the following functions?

```
not
isDigit
toUpper
ord
chr
```

---

# Function application

What is the syntax for function application in Haskell?

---

# Functions of multiple arguments

How about functions of multiple arguments?

E.g., interpret the following functions that map from a `Bool` and a `Char` to
an `Int`:

```haskell
foo1 :: (Bool, Char) -> Int

foo2 :: Bool -> (Char -> Int)

foo3 :: Bool -> Char -> Int
```

---

Functions of multiple arguments in Haskell are "curried".

- What does this mean?

- What does this say about the associativity of `->`?

- What does this say about the associativity of function application?

- What happens if we "partially apply" a function of multiple arguments?

Aside: what about:

```haskell
foo4 :: (Bool -> Char) -> Int
```

---

# "Operators"

Operators are just functions whose names start with non-letters, and are used
(by default) in infix form (e.g., `13 + 25`)

- You can ask for information about an operator's type at GHCi using `:i`

  - Also includes information about the precedence and associativity

  - Note: function application has the highest precedence!

- Check out some operators:

```
+
*
^
**
&&
==
/=
```

- Operators can be used in prefix form if we put them in parentheses (try it!)

- Functions can be used in infix form (try it with `mod` and `gcd`)

---

# Polymorphic functions

- What are polymorphic functions?

- What do their type declarations look like?

Check out these polymorphic functions? Can you guess what they do?

```
id
const
fst
snd
.
flip
```

The type declaration of a polymorphic function can give a lot of information
about what the function does! (Why?)

---

# Type Classes (aka Classes)

- What is a type class?

- What information is specified by a type class?

Check out these classes and their methods:

```
Eq
Ord
Num
Enum
Integral
Bounded
Show
```

---

# Class constraints

- What is a class constraint?

- Why are class constraints useful?

Inspect and explain the type declarations for:

```
^
exp
/=
<
fromIntegral
read
sort
lookup
```
