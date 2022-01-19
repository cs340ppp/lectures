% CS 340: Programming Paradigms and Patterns
% Lect 08 - Defining Types and Type Classes
% Michael Lee

> {-# LANGUAGE FlexibleInstances #-}
> module Lect08 where
> import Prelude hiding (Word, Maybe, Just, Nothing, Either, Left, Right)
> import Data.Char

Defining Types and Type Classes
===============================

Agenda:
  - Type synonyms
  - Algebraic data types
  - Polymorphic types
  - Type Classes


Type synonyms
-------------

`type` defines type synonyms, i.e., alternative names for existing types. Note
that all type names must be capitalized.

> type Letter = Char
> type Word = [Letter]
> type Sentence = [Word]
>
> sentences :: [Word] -> [Word] -> [Word] -> [Sentence]
> sentences subjs verbs objs = [[s,v,o] | s <- subjs, v <- verbs, o <- objs]

> type Point2D = (Double, Double)
> 
> distance :: Point2D -> Point2D -> Double
> distance (x1,y1) (x2,y2) = sqrt $ (x1-x2)^2 + (y1-y2)^2


Algebraic data types
--------------------

The `data` keyword is used to define new types. In the definition of the type we
list one or more value constructors (aka data constructors) that would be used
to create values of this type. Value constructor names must also be capitalized.

E.g., the `YesOrNo` type below has two value constructors: `Yes` and `No`:

> data YesOrNo = Yes | No deriving Show

(the `deriving` clause allows values of this type to be displayed in GHCi 
and converted to strings --- we'll clarify this later)

A value constructor is essentially a function that returns a value of the
defined type. We can also pattern match against value constructors in 
functions:

> yn1 :: YesOrNo
> yn1 = Yes
>
> yn2 :: YesOrNo
> yn2 = No
>
> not' :: YesOrNo -> YesOrNo
> not' Yes = No
> not' No  = Yes
>
> (|||) :: YesOrNo -> YesOrNo -> YesOrNo
> (|||) = undefined
>
> or' :: [YesOrNo] -> YesOrNo
> or' = undefined

---

In the type definition, value constructors can also be followed by field types.

E.g., `Box` has a single value constructor (also named `Box` --- this is ok
because type names and functions are in separate namespaces) with three fields:

> data Box = Box Int Bool String deriving Show

To construct a `Box`, we pass its value constructor values corresponding to the
field types. 

> b1 = Box 5 True "hello"
> b2 = Box 100 False "goodbye"

When pattern matching, we can also deconstruct values into their fields:

> boxStr :: Box -> String
> boxStr = undefined
>
> boxCombine :: Box -> Box -> Box
> boxCombine = undefined

---

We can have multiple value constructors with varying numbers of fields. 

E.g., `Shape` has three value constructors, each with one or more fields:

> data Shape = Circle Double 
>              | Triangle Double Double 
>              | Rectangle Double Double

Pattern matching lets us differentiate between different values of a given type,
and extract their constituent fields:

> area :: Shape -> Double
> area (Circle r) = pi * r^2
> area (Triangle h b) = (h*b)/2
> area (Rectangle l w) = l*w

---

We call these "algebraic" data types because data types defined in this way can
be formed from the "sum" and "product" of other types.

Here are two sum types:

> data T1 = T1V1 | T1V2 | T1V3
> data T2 = T2V1 Bool | T2V2 T1

To determine the values that make up either `T1` or `T2`, we just "sum up" the
values that can be created using all their respective value constructors. How
many values make up `T2`?

Here's a product type:

> data T3 = T3V Bool T1

To determine the values that make up `T3`, we compute the "product" of the
values for the constituent types of its single value constructor. How many
values make up `T3`?

Here's a type that is both a sum and product type:

> data T4 = T4V1 T1 T2 | T4V2 T2 T3

How many values make up `T4`?

---

We can also use "record" syntax to define attribute names and automatically
generate "getter" functions:

> data Student = Student {
>   firstName :: String,
>   lastName  :: String,
>   studentId :: Integer,
>   grades    :: [Char]
> } deriving Show

We can still create values with fields specified positionally:

> s1 = Student "John" "Doe" 1234 ['A', 'B']

Or we can specify fields by name (order doesn't matter):

> s2 = Student { lastName = "Doe", 
>                firstName = "Jane",
>                grades = ['A', 'C'],
>                studentId = 2345 }

Record syntax also provides a shortcut for creating a new value from another:

> s3 = s1 { grades = ['B', 'A', 'D'] }

---

We can also define *self-referential* types --- i.e., a type where one or more
value constructors reference the type being defined.

> data RussianDoll = RussianDoll String RussianDoll | EmptyDoll
>                    deriving Show

Here are some `RussianDoll`s:

> d1 = EmptyDoll
> d2 = RussianDoll "privyet" EmptyDoll
> d3 = RussianDoll "matry" (RussianDoll "osh" (RussianDoll "ka" EmptyDoll))
> d4 = RussianDoll "and on and on" d4

Write a function to return the message in the innermost non-empty doll:

> innerMostMessage :: RussianDoll -> String
> innerMostMessage = undefined

Write a function to reverse all messages in a doll:

> reverseMessages :: RussianDoll -> RussianDoll
> reverseMessages = undefined


Polymorphic Types
-----------------

A polymorphic type is a type defined using one or more type variables.

E.g., here is a box type parameterized by a single type variable:

> data UniversalBox a = UBox a deriving Show

The type name, `UniversalBox`, is now a *type constructor*. We must provide it
with a data type `T` to "specialize" it as `UniversalBox T`, which has a value
constructor `UBox` that takes a value of type `T`.

E.g., here are some different `UniversalBox` values:

> ub1 :: UniversalBox Bool
> ub1 = UBox True
>
> ub2 :: UniversalBox [Int]
> ub2 = UBox [1..10]
>
> ub4 :: Num a => UniversalBox (a -> a)
> ub4 = UBox (\x -> 2*x)


E.g., let's define some some functions on `UniversalBox` values:

> boxStrCat :: UniversalBox String -> UniversalBox String -> UniversalBox String
> boxStrCat = undefined

> boxComp :: Ord a => UniversalBox a -> UniversalBox a -> Ordering
> boxComp = undefined


We say that the `UniversalBox` type constructor has "kind" (* -> *), where *
denotes a monomorphic type. I.e., the type constructor takes a monomorphic type
and produces a monomorphic type. Note that all values have types of kind *. 

---

A polymorphic type defined in Prelude is `Maybe`, defined as:

> data Maybe a = Just a | Nothing deriving Show

We use `Maybe` to create types that can represent both a value or the absence of
a value. This allows us to write functions with well-defined types that can
return values that represent "failed" computations (without a magical "null"
return value or by raising an exception, as might be done in other languages).

E.g., consider:

> quadRoots :: Double -> Double -> Double -> Maybe (Double,Double)
> quadRoots a b c = let d = b^2-4*a*c
>                       sd = sqrt d
>                   in if d < 0
>                      then Nothing
>                      else Just ((-b+sd)/(2*a), (-b-sd)/(2*a))
> 
> find :: (a -> Bool) -> [a] -> Maybe a
> find _ [] = Nothing
> find p (x:xs) | p x = Just x
>               | otherwise = find p xs
>
> idLookup :: Integer -> String
> idLookup id = case find ((== id) . fst) db of
>                 Nothing -> "Not found"
>                 Just (_, name) -> name
>   where db = [(1234, "John Doe"), 
>               (2345, "Jane Doe"), 
>               (3456, "Mary Doe")]

---

Another polymorphic type found in Prelude is `Either`, defined as:

> data Either a b = Left a | Right b deriving Show

We often use `Either` to create data types where the `Left` constructor
contains error values, and the `Right` constructor contains correct values.

> find' :: (a -> Bool) -> [a] -> Either String a
> find' _ [] = Left "List was empty"
> find' p (x:xs) | p x = Right x
>                | null xs = Left "No element satisifying predicate"
>                | otherwise = find' p xs

Note that the `Either` type has kind (* -> * -> *), as its type constructor
takes two data types as parameters to fully specialize it.

It is also possible to define higher-order type constructors (i.e., that take
other type constructors), e.g.,

  (* -> *) -> * -> *

A type with the above kind is:

> data T a b = T (a b)

The :kind command in GHCi can be used to reveal the kind of any type.

---

The built-in list is just another polymorphic type! We can define our own list
type like this:

> data List a = Cons a (List a) | Empty deriving Show

Here are some lists:

> l1 :: List Char
> l1 = Cons 'h' (Cons 'e' (Cons 'l' (Cons 'l' (Cons 'o' Empty))))
>
> l2 :: List Char
> l2 = Cons 'h' (Cons 'a' l2)
>
> l3 :: List (List Int)
> l3 = Cons (Cons 1 (Cons 2 Empty)) (Cons (Cons 3 (Cons 4 Empty)) Empty)

Let's define some list functions!

> takeL :: Int -> List a -> List a
> takeL = undefined
>
> mapL :: (a -> b) -> List a -> List b
> mapL = undefined
>
> foldrL :: (a -> b -> b) -> b -> List a -> b
> foldrL = undefined

---

There's nothing stopping us from defining arbitrarily complex, self-referential
data types. E.g., here's a tree type:

> data Tree a = Node a [Tree a] | Leaf a deriving Show

Which can be used to model hierarchical data:

> t1 :: Tree String
> t1 = Node "Animals" [
>        Leaf "Arthropods", 
>        Node "Chordates" [ Leaf "Birds", Leaf "Mammals", Leaf "Reptiles" ],
>        Leaf "Nematodes"
>      ]

Type Classes
------------

A type class defines a collection of functions associated with conforming types.

Types that conform to a type class are called *instances* of that class, and the
functions defined by the class are called *methods*.

Here's a silly class that defines two methods:

> class Explosive a where
>   explode :: a -> [a]

The above says that for all types `a` that are instances of `Explosive`, there
is an `explode` function that takes that type and returns a list of that type. 

To make a type an instance of a class, we need to declare it so and implement
the requisite method(s):

> instance Explosive Integer where
>   explode = undefined
>
> instance Explosive Char where
>   explode = undefined
>
> instance Explosive [a] where
>   explode = undefined

Now we can apply `explode` to `Integer`, `Char`, and list values. Looking at the
output of ":t explode" in GHCi confirms that `explode` has the constrained type:

    explode :: Explosive a => a -> [a]

We can also use the class as a constraint in other functions:

> blowItAllUp :: Explosive a => [a] -> [[a]]
> blowItAllUp = undefined

---

Consider the class `Eq`, defined thusly in Prelude:

    class Eq a where
      (==) :: a -> a -> Bool
      x == y = not (x /= y)
      
      (/=) :: a -> a -> Bool
      x /= y = not (x == y)


Note that it also contains definitions for its methods --- `==` and `/=` are
defined in terms of each other. This makes it so that an instance need only
define one of the methods; the missing method will default to the implementation
found in the class.

E.g., make the `Student` type defined earlier (show below) an instance of `Eq`:

    data Student = Student {
      firstName :: String,
      lastName  :: String,
      studentId :: Integer,
      grades    :: [Char]
    } 

> instance Eq Student where
>   (==) = undefined

---

Here is the definition of class `Ord`, also from Prelude:

    class (Eq a) => Ord a where
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


The `(Eq a) => Ord a` means that `Ord` is a subclass of `Eq`; i.e., that `Ord`
inherits all the methods of `Eq`, and that all instances of `Ord` must also
be instances of `Eq`.

Because of all the method implementations found in `Ord`, a minimal instance
need only supply either the `compare` or `<=` methods. 

E.g., make `Student` an instance of `Ord`:

> instance Ord Student where
>   compare = undefined

Making a polymorphic type an instance of a class may require adding constraints
to the instance declaration. E.g., complete our `List` instance of `Eq`:

> instance (Eq a) => Eq (List a) where
>   (==) = undefined


-- Automatic derivation

The Haskell compiler can automagically derive instances of the classes Eq, Ord,
Enum, Bounded, Show, and Read for user defined data types (which meet certain
criteria) by using the `deriving` clause. This makes it easier to compare,
enumerate, print/parse data when the default behavior is sufficient.

E.g.,

> data Suit = Diamond | Club | Heart | Spade 
>             deriving (Eq, Ord, Enum, Bounded, Show, Read)
