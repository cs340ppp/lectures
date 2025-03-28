---
title: Defining Types
author: Michael Lee
---

# Agenda

- Introduction
- Type synonyms
- Algebraic data types
- Self-referential types
- Parametric types
  - Kinds of types
  - Some useful parametric types

---

# Introduction

- Up until now, we've only studied *pre-existing* data types and classes
  - Concrete types, e.g., `Bool`, `Int`, `Char`, `String`, `(Int, Bool)`
  - Polymorphic types, e.g., `(a,b)`, `[a]`
  - Type classes, e.g., `Eq`, `Ord`, `Num`

- Time to build our own!

---

# Type synonyms

"`type`" defines a *type synonym*

- new name for an existing type

- often used as a form of documentation / to improve legibility

---

# Type synonyms

## E.g., English terms vs. Chars/Strings

```haskell
type Letter = Char

type Word = [Letter]

type Sentence = [Word]
```

<!-- pause -->

```haskell
phrases :: [Word] -> [Word] -> [Sentence]
phrases adjs nouns = [ [adj, noun]
                       | adj <- adjs, noun <- nouns ]
```

---

# Type synonyms

## E.g., Tuple aliases

(x,y) coordinates:

```haskell
type Point2D = (Double, Double)

distance :: Point2D -> Point2D -> Double
distance (x1,y1) (x2,y2) = sqrt $ (x1-x2)^2 + (y1-y2)^2
```

<!-- pause -->

Attaching numbers to values:

```haskell
type Numbered a = (Int, a)

enumerate :: [a] -> [Numbered a]
enumerate xs = zip [1..] xs
```

---

# *Algebraic* data types

"`data`" defines a new *algebraic data type* with at least one *value
constructor* (aka *data constructor*)

```haskell
data TypeName = Constructor1 | Constructor2 | ... | ConstructorN
```

<!-- pause -->

Each constructor can take arguments, which describe the types of values that
must be passed to the constructor when calling it.

```haskell
data TypeName = Constructor1 ArgType1 | Constructor2 ArgType1 ArgType2 | ...
```

---

# *Algebraic* data types

## `YesOrNo` type

```haskell
data YesOrNo = Yes | No
```

<!-- pause -->

- we can pattern match on value constructors:

```haskell
not :: YesOrNo -> YesOrNo
not Yes = No
not No  = Yes

(||) :: YesOrNo -> YesOrNo -> YesOrNo
No || No = No
_  || _  = Yes
```

---

# *Algebraic* data types

## `Box` type

A `Box` value is built of an `Int`, `Bool`, and `String`.

```haskell
data Box = Box Int Bool String
```

<!-- pause -->

- The name of the type (`Box`) and value constructor (`Box`) are identical.

  - No ambiguity; they are always used in different contexts!

  - What is the type of the value constructor?

---

# *Algebraic* data types

## `Box` type

Working with `Box` values:

```haskell
data Box = Box Int Bool String

myBox :: Box
myBox = Box 5 True "hello"

boxAdd :: Box -> Box -> Box
boxAdd (Box i1 b1 s1) (Box i2 b2 s2)
       = Box (i1 + i2) (b1 || b2) (s1 ++ s2)
```

- Identify appearances of the `Box` type name and value constructor.

---

# *Algebraic* data types

## `Shape` type

`Shape` is a type whose values represent measurements of different shapes.

```haskell
data Shape = Circle Double 
             | Triangle Double Double 
             | Rectangle Double Double
```

<!-- pause -->

Compute the area of a shape:

```haskell
area :: Shape -> Double
area (Circle r)      = pi * r^2
area (Triangle h b)  = (h*b)/2
area (Rectangle l w) = l*w
```

---

# *Algebraic* data types

## Why "*Algebraic*"?

An algebraic type is based on the *sum* and *product* of constituent values and
types.

Here are two sum types:

```haskell
data T1 = T1V1 | T1V2 | T1V3
data T2 = T2V1 Bool | T2V2 T1
```

- How many values belong to the types `T1` and `T2`?

<!-- pause -->

Here is a product type:

```haskell
data T3 = T3V Bool T1
```

How many values belong to the type `T3`?

---

# *Algebraic* data types

## Why "*Algebraic*"?

```haskell
data T1 = T1V1 | T1V2 | T1V3
data T2 = T2V1 Bool | T2V2 T1
data T3 = T3V Bool T1
```

Here is a type that is both a sum and product type:

```haskell
data T4 = T4V1 T1 T2 | T4V2 T2 T3
```

How many values make up `T4`?

---

# Record syntax

Aternate way of defining algebraic types -- allows us to specify attribute
names, and automatically generates "getter" functions:

```haskell
data Student = Student { firstName :: String
                       , lastName  :: String
                       , studentId :: Integer
                       , grades    :: [Char] }
```

<!-- pause -->

We can still create values with fields specified positionally:

```haskell
s1 = Student "John" "Doe" 1234 ['A', 'B']
```

<!-- pause -->

Or we can specify fields by name (order doesn't matter):

```haskell
s2 = Student { lastName = "Doe"
             , firstName = "Jane"
             , grades = ['A', 'C']
             , studentId = 2345 }
```

<!-- pause -->

We also get a shortcut for creating a new value from another:

```haskell
s3 = s1 { grades = ['B', 'A', 'D'] }
```

---

# Self-referential types

How would you interpret the following type?

```haskell
data RussianDoll = Doll String RussianDoll | EmptyDoll
```

<!-- pause -->

Here are some `RussianDoll` values:

```haskell
d1 = EmptyDoll
d2 = Doll "privyet" EmptyDoll
d3 = Doll "matry" (Doll "osh" (Doll "ka" EmptyDoll))
d4 = Doll "infinity, and beyond!" d4
```

<!-- pause -->

Write a function to return the message in the innermost non-empty doll:

```haskell
innerMostMessage :: RussianDoll -> String
innerMostMessage EmptyDoll          = error "No message"
innerMostMessage (Doll m EmptyDoll) = m
innerMostMessage (Doll _ d)         = innerMostMessage d
```

---

# Parametric types

A type definition can depend on one or more type variables. Such types are known
as *parametric* (or *parameterized*) types.

<!-- pause -->

E.g., a "universal" box type:

```haskell
data UniversalBox a = UBox a
```

The type name, `UniversalBox`, is not itself a full type specification. It takes
another type as a parameter to specialize it; we therefore call it a *type
constructor*.

<!-- pause -->

E.g., here are some `UniversalBox` values:

```haskell
ub1 :: UniversalBox Bool
ub1 = UBox True

ub2 :: UniversalBox [Int]
ub2 = UBox [1..10]

ub3 :: UniversalBox (Int -> Bool)
ub3 = UBox even
```

---

# Parametric types

E.g., some functions on `UniversalBox` values:

```haskell
mapBox :: (a -> b) -> UniversalBox a -> UniversalBox b
mapBox f (UBox x) = UBox $ f x

sumBoxes :: Num a => [UniversalBox a] -> UniversalBox a
sumBoxes [] = UBox 0
sumBoxes (UBox n : bs) = let UBox ns = sumBoxes bs 
                         in UBox $ n + ns
```

---

# *Kinds* of Types

Type constructors vary in the number of type parameters they have. We categorize
types by *kind*, which indicate what their type constructors can take as
arguments.

- When a type constructor takes no parameters, we say that it is a *concrete
  type*, denoted kind `*`.

- A type constructor that takes one concrete type in order to yield a fully
  specialized type is denoted kind `* -> *`

<!-- pause -->

What are the *kinds* of the following types? (use `:k` in ghci)

```haskell
data Bar a b = Bar1 a | Bar2 b
data Bat a b c = Bat a b c
data Baz a b = Baz (a b)
```

---

# Some useful parametric types

## `Maybe`

## `Either`

## Lists

---

## `Maybe`

A `Maybe a` represents either an `a` value or the *absence of an `a` value*.

```haskell
data Maybe a = Just a | Nothing
```

<!-- pause -->

This gives us a well-typed mechanism for returning a value from a function that
represents a result or the *lack of a result* (e.g., a failed computation).

<!-- pause -->

E.g., consider:

```haskell
find :: (a -> Bool) -> [a] -> Maybe a
find _ [] = Nothing
find p (x:xs) | p x = Just x
              | otherwise = find p xs
```

---

## `Either`

`Either` has constructors that take one of two types.

```haskell
data Either a b = Left a | Right b
```

<!-- pause -->

We typically use the `Left` constructor to represent error values, and the
`Right` constructor to hold correct (*right*) values.

```haskell
find' :: (a -> Bool) -> [a] -> Either String a
find' _ [] = Left "List was empty"
find' p (x:xs) | p x = Right x
               | null xs = Left "No matching element"
               | otherwise = find' p xs
```

---

## Lists

The built-in list is just another parametric type! We can define our own list
type like this (`:-` is a value constructor):

```haskell
infixr 5 :-
data List a = a :- (List a) | Null
```

<!-- pause -->

Here are some lists:

```haskell
l1 :: List Char
l1 = 'h' :- 'e' :- 'l' :- 'l' :- 'o' :- Null

l2 :: List (List Int)
l2 = (1 :- 2 :- Null) :- (3 :- 4 :- Null) :- Null
```

<!-- pause -->

And here's a map for our list!

```haskell
mapL :: (a -> b) -> List a -> List b
mapL _ Null = Null
mapL f (x :- xs) = f x :- mapL f xs
```
