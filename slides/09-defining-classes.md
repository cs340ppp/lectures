---
title: "Defining and Using Type Classes"
sub_title: "CS 340: Programming Patterns and Paradigms"
author: "Michael Lee <lee@iit.edu>"
---

# Agenda

- Review of Type classes
- Some classes and their instances:
  - Explosive
  - Eq
  - Ord
  - Foldable
- Automatic derivation

---

# Type Classes

A type class defines a collection of functions implemented by conforming types.

- types that conform to a type class are called *instances* of that class
- the functions defined by the class are called its *methods*

Classes enable *ad hoc polymorphism*.

---

# Example: `Explosive`

```haskell
class Explosive a where
  explode :: a -> [a]
```

- all types `a` that are instances of `Explosive` must implement `explode`,
  which takes that type and returns a list of that type.

<!-- pause -->

Here is an `Int` instance of `Explosive`:

```haskell
instance Explosive Int where
  explode :: Int -> [Int]
  explode n = [1..n]
```

<!-- pause -->

And a list instance:

```haskell
instance Explosive [a] where
  explode :: [a] -> [[a]]
  explode l = map (:[]) l
```

<!-- pause -->

And now we can do:

```haskell
explode 5  ==  [1,2,3,4,5]
explode [1..5]  ==  [[1],[2],[3],[4],[5]]
```

---

# Example: `Explosive`

## As a class constraint

We can use `Explosive` as a class constraint for polymorphic functions:

```haskell
mapExploded :: Explosive a => (a -> b) -> a -> [b]
mapExploded f x = map f $ explode x
```

<!-- pause -->

And `mapExploded` will work for all `Explosive` types!

```haskell
mapExploded even 5  ==  [False,True,False,True,False]
mapExploded length [1..5]  ==  [1,1,1,1,1]
```

---

# Example: `Explosive`

## Default implementations

We added another method, `explodeTwice`, with a *default implementation*.

```haskell
class Explosive a where
  explode :: a -> [a]

  explodeTwice :: a -> [[a]]
  explodeTwice = map explode . explode
```

<!-- pause -->

- instances get the defaults "for free", but may override them

```haskell
explode 5  ==  [1,2,3,4,5]
explodeTwice 5  ==  [[1],[1,2],[1,2,3],[1,2,3,4],[1,2,3,4,5]]
```

---

# Example: `Eq`

Defines methods to test for equality/inequality:

```haskell
class Eq a where
  (==) :: a -> a -> Bool
  x == y = not (x /= y)
  
  (/=) :: a -> a -> Bool
  x /= y = not (x == y)
```

- `==` and `/=` are defined, but in terms of each other!

- An instance needs to define only one of the methods (*minimal implementation*)

---

# Example: `Eq`

## `Student` instance

Can you make the `Student` type an instance of `Eq`?

```haskell
data Student = Student {
               , firstName :: String
               , lastName  :: String
               , studentId :: Integer
               , grades    :: [Char]
               }
```

<!-- pause -->

Simple implementation based solely on `studentId` field:

```haskell
instance Eq Student where
  (==) :: Student -> Student -> Bool
  (Student _ _ id1 _) == (Student _ _ id2 _) = id1 == id2
```

---

# Example: `Eq`

## `List` instance

How would you make `List` an instance of `Eq`?

```haskell
data List a = a :- (List a) | Null
```

<!-- pause -->

Making a polymorphic type an instance of a class may require adding constraints
to the instance declaration.

```haskell
instance Eq a => Eq (List a) where
  (==) :: Eq a => List a -> List a -> Bool
  Null      == Null      = True
  (x :- xs) == (y :- ys) = x == y && xs == ys
  _         == _         = False
```

- `Eq a => Eq (List a)` means `List a` can be an instance of `Eq` only if `a` is
  an instance of `Eq`.

---

# Example: `Ord`

Relational methods:

```haskell
class Eq a => Ord a where
    compare              :: a -> a -> Ordering
    (<), (<=), (>), (>=) :: a -> a -> Bool
    max, min             :: a -> a -> a

    compare x y = if x == y then EQ
                  else if x <= y then LT
                  else GT

    x <  y = case compare x y of { LT -> True;  _ -> False }
    x <= y = case compare x y of { GT -> False; _ -> True }
    x >  y = case compare x y of { GT -> True;  _ -> False }
    x >= y = case compare x y of { LT -> False; _ -> True }

    max x y = if x <= y then y else x
    min x y = if x <= y then x else y
```

<!-- pause -->

- `Eq a => Ord a` means `Eq` is the *superclass* of `Ord`
  - `Ord` inherits all methods of `Eq`
  - All instances of `Ord` must also be instances of `Eq`
  - A type class can have multiple superclasses!

- A minimal instance only needs to implement `compare` or `<=`

---

# Example: `Ord`

## `Student` instance

```haskell
instance Ord Student where
  compare :: Student -> Student -> Ordering
  compare (Student _ _ id1 _) (Student _ _ id2 _)
      = compare id1 id2
```

---

# Example: `Foldable`

Methods built atop primitive recursion:

```haskell
class Foldable t where
  foldr :: (a -> b -> b) -> b -> t a -> b
  foldl :: (b -> a -> b) -> b -> t a -> b
  foldr1 :: (a -> a -> a) -> t a -> a
  foldl1 :: (a -> a -> a) -> t a -> a
  toList :: t a -> [a]
  null :: t a -> Bool
  length :: t a -> Int
  elem :: Eq a => a -> t a -> Bool
  maximum :: Ord a => t a -> a
  minimum :: Ord a => t a -> a
  sum :: Num a => t a -> a
  product :: Num a => t a -> a
```

- `foldr` is the only required method!

---

# Example: `Foldable`

## `List` instance

```haskell
instance Foldable List where
  foldr :: (a -> b -> b) -> b -> List a -> b
  foldr _ v Null = v
  foldr f v (x :- xs) = f x $ foldr f v xs
```

What functionality does this give us?

---

# Automatic derivation

The Haskell compiler can automagically derive instances of the classes `Eq`,
`Ord`, `Enum`, `Bounded`, `Show`, and `Read` for data types (which meet certain
criteria) by using the `deriving` clause.

This makes it easier to compare, enumerate, print/parse data when the default
behavior is sufficient.

E.g.,

```haskell
data Suit = Diamond | Club | Heart | Spade 
            deriving (Eq, Ord, Enum, Bounded, Show, Read)
```

- Play around with this type in GHCi to see what behavior we get for free!
  (`:i`-ing the classes will help)
