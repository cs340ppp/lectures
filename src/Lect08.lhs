% CS 340: Programming Paradigms and Patterns
% Lect 08 - Defining Types and Type Classes
% Michael Lee

\begin{code}
{-# LANGUAGE FlexibleInstances #-}
module Lect08 where
import Prelude hiding (Word, Maybe, Just, Nothing, Either, Left, Right)
import Data.Char
\end{code}

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


\begin{code}
type Letter = Char

type Word = [Letter]

type Sentence = [Word]

sentences :: [Word] -> [Word] -> [Word] -> [Sentence]
sentences subjs verbs objs = [[s,v,o] | s <- subjs, v <- verbs, o <- objs]

type Point2D = (Double, Double)

distance :: Point2D -> Point2D -> Double
distance (x1,y1) (x2,y2) = sqrt $ (x1-x2)^2 + (y1-y2)^2
\end{code}


Algebraic data types
--------------------

The `data` keyword is used to define new types. A type definition requires 
one or more *value constructors* (aka data constructors).

E.g., the `YesOrNo` type below has two value constructors: `Yes` and `No`:

\begin{code}
data YesOrNo = Yes | No deriving Show
\end{code}

(the `deriving` clause allows values of this type to be displayed in GHCi 
and converted to strings -- more on this later)


We can pattern match on value constructors. Implement:

\begin{code}
not' :: YesOrNo -> YesOrNo
not' = undefined


(|||) :: YesOrNo -> YesOrNo -> YesOrNo
(|||) = undefined


or' :: [YesOrNo] -> YesOrNo
or' = undefined
\end{code}

---

In a type definition, value constructors may be followed by field types.

E.g., consider the following type (note that type names and value constructor names can be the same):

\begin{code}
data Box = Box Int Bool String deriving Show
\end{code}

Let's construct some Boxes:

\begin{code}
b1 = Box 5 True "hello"
b2 = Box 100 False "goodbye"
\end{code}


Use pattern matching to write some Box functions:

\begin{code}
boxStr :: Box -> String
boxStr = undefined

boxCombine :: Box -> Box -> Box
boxCombine = undefined
\end{code}

---

We can have multiple value constructors with varying numbers of fields. 

E.g., `Shape` has three value constructors, each with one or more fields:

\begin{code}
data Shape = Circle Double 
             | Triangle Double Double 
             | Rectangle Double Double deriving Show
\end{code}

Pattern matching lets us differentiate between different values of a given type,
and extract their fields. Try implementing:

\begin{code}
area :: Shape -> Double
area = undefined
\end{code}

---

We call these "algebraic" data types because data types defined in this way can
be formed from the "sum" and "product" of other types.


Here are two sum types:

\begin{code}
data T1 = T1V1 | T1V2 | T1V3
data T2 = T2V1 Bool | T2V2 T1
\end{code}

How many values belong to type `T2`?


Here's a product type:

\begin{code}
data T3 = T3V Bool T1
\end{code}


How many values belong to type `T3`?


Here's a type that is both a sum and product type:

\begin{code}
data T4 = T4V1 T1 T2 | T4V2 T2 T3
\end{code}

How many values belong to type `T4`?

---

We may use "record" syntax to define attribute names and automatically
generate "getter" functions:

\begin{code}
data Student = Student { firstName :: String
                       , lastName  :: String
                       , studentId :: Integer
                       , grades    :: [Char]
                       } deriving Show
\end{code}


We can still create values with fields specified positionally:

\begin{code}
s1 = Student "John" "Doe" 1234 ['A', 'B']
\end{code}


Or we can specify fields by name (order doesn't matter):

\begin{code}
s2 = Student { lastName = "Doe"
             , firstName = "Jane"
             , grades = ['A', 'C']
             , studentId = 2345 }
\end{code}


Record syntax provides a shortcut for creating a new value from another:

\begin{code}
s3 = s1 { grades = ['B', 'A', 'D'] }
\end{code}

---

We may define *self-referential* types --- i.e., a type where one or more
value constructors reference the type being defined.

\begin{code}
data RussianDoll = RussianDoll String RussianDoll | EmptyDoll
                   deriving Show
\end{code}


Here are some `RussianDoll`s:

\begin{code}
d1 = EmptyDoll
d2 = RussianDoll "privyet" EmptyDoll
d3 = RussianDoll "matry" (RussianDoll "osh" (RussianDoll "ka" EmptyDoll))
d4 = RussianDoll "and on and on" d4
\end{code}


Write a function to return the message in the innermost non-empty doll:

\begin{code}
innerMostMessage :: RussianDoll -> String
innerMostMessage = undefined
\end{code}


Write a function to reverse the order of messages in a doll:

\begin{code}
reverseMessages :: RussianDoll -> RussianDoll
reverseMessages = undefined
\end{code}


Polymorphic Types
-----------------

A polymorphic type is a type defined using one or more type variables.

E.g., here is a box type parameterized by a single type variable:

\begin{code}
data UniversalBox a = UBox a deriving Show
\end{code}

The type name, `UniversalBox`, is now a *type constructor*. We must provide it
with a data type `T` to "specialize" it as `UniversalBox T`, which has a value
constructor `UBox` that takes a value of type `T`.

E.g., here are some different `UniversalBox` values:

\begin{code}
ub1 :: UniversalBox Bool
ub1 = UBox True

ub2 :: UniversalBox [Int]
ub2 = UBox [1..10]

ub4 :: Num a => UniversalBox (a -> a)
ub4 = UBox (\x -> 2*x)
\end{code}


E.g., define some some functions on `UniversalBox` values:

\begin{code}
catBoxes :: UniversalBox [a] -> UniversalBox [a] -> UniversalBox [a]
catBoxes = undefined


sumBoxes :: Num a => [UniversalBox a] -> UniversalBox a
sumBoxes = undefined
\end{code}


We say the `UniversalBox` type constructor has "kind" (* -> *), where * denotes
a monomorphic type. Note that all concrete values have types of kind *. 

---

A polymorphic type defined in Prelude is `Maybe`, defined as:

\begin{code}
data Maybe a = Just a | Nothing deriving Show
\end{code}

We use `Maybe` to create types that can represent both a value or the absence of
a value (or error).

E.g., rewrite the following functions using `Maybe`:

\begin{code}
quadRoots :: Double -> Double -> Double -> (Double,Double)
quadRoots a b c = let d = b^2-4*a*c
                      sd = sqrt d
                  in if d < 0
                     then error "No real roots"
                     else ((-b+sd)/(2*a), (-b-sd)/(2*a))


find :: (a -> Bool) -> [a] -> a
find _ [] = error "Value not found"
find p (x:xs) | p x = x
              | otherwise = find p xs
\end{code}

---

Another polymorphic type found in Prelude is `Either`, defined as:

\begin{code}
data Either a b = Left a | Right b deriving Show
\end{code}

We often use `Either` to create data types where the `Left` constructor
contains error values, and the `Right` constructor contains correct values.


E.g., rewrite the following using `Either`:

\begin{code}
find' :: (a -> Bool) -> [a] -> a
find' _ [] = error "List was empty"
find' p (x:xs) | p x = x
               | null xs = error "No element satisifying predicate"
               | otherwise = find' p xs
\end{code}

Note that the `Either` type has kind (* -> * -> *), as its type constructor
takes two data types as parameters to fully specialize it.

It is also possible to define higher-order type constructors (i.e., that take
other type constructors).

E.g., what is the kind of the following type? (try using :kind and :t on T)

\begin{code}
data T a b = T (a b)
\end{code}

---

The built-in list is just another polymorphic type! We can define our own list
type like this (we're using `:-` as a value constructor):

\begin{code}
infixr 5 :-
data List a = a :- (List a) | Null deriving Show
\end{code}


Here are some lists:

\begin{code}
l1 :: List Char
l1 = 'h' :- 'e' :- 'l' :- 'l' :- 'o' :- Null

l2 :: List Char
l2 = 'h' :- 'a' :- Null

l3 :: List (List Int)
l3 = (1 :- 2 :- Null) :- (3 :- 4 :- Null) :- Null
\end{code}


Let's define some list functions!

\begin{code}
enumFromToL :: (Eq a, Enum a) => a -> a -> List a
enumFromToL = undefined


enumFromL :: (Eq a, Enum a) => a -> List a
enumFromL = undefined


takeL :: Int -> List a -> List a
takeL = undefined
\end{code}


Type Classes
------------

A type class defines a collection of functions associated with conforming types.

Types that conform to a type class are called *instances* of that class, and the
functions defined by the class are called *methods*.

Here's a class that defines one method:

\begin{code}
class Explosive a where
  explode :: a -> [a]
\end{code}


To make a type an instance of a class, we need implement the needed method(s).
Define the following instances of `Explosive`.

\begin{code}
instance Explosive Integer where
  explode :: Integer -> [Integer]
  explode = undefined

instance Explosive [a] where
  explode :: [a] -> [[a]]
  explode = undefined
\end{code}


---

Consider the class `Eq`, defined thusly in Prelude:

\begin{verbatim}
class Eq a where
  (==) :: a -> a -> Bool
  x == y = not (x /= y)
  
  (/=) :: a -> a -> Bool
  x /= y = not (x == y)
\end{verbatim}

It provides `==` and `/=`, which are defined in terms of each other. So an
instance only needs to define one of the methods!


E.g., make the `Student` type defined earlier (show below) an instance of `Eq`:

    data Student = Student {
      firstName :: String,
      lastName  :: String,
      studentId :: Integer,
      grades    :: [Char]
    } 

\begin{code}
instance Eq Student where
  (==) :: Student -> Student -> Bool
  (Student _ _ id1 _) == (Student _ _ id2 _) = id1 == id2
\end{code}

---

Here is the definition of class `Ord`, which inherits all the methods from `Eq` 
(i.e., `Ord` is a subclass of `Eq`):

\begin{verbatim}
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
\end{verbatim}

An instance only needs to define either the `compare` or `<=` methods. 

E.g., make `Student` an instance of `Ord`:

\begin{code}
instance Ord Student where
  compare :: Student -> Student -> Ordering
  compare (Student _ _ id1 _) (Student _ _ id2 _) = compare id1 id2
\end{code}


Making a polymorphic type an instance of a class may require adding constraints
to the instance declaration. E.g., complete our `List` instance of `Eq`:

\begin{code}
instance (Eq a) => Eq (List a) where
  (==) :: Eq a => List a -> List a -> Bool
  (==) = undefined
\end{code}

---

Another useful class is `Foldable`, which only require `foldr` to be defined:

\begin{verbatim}
class Foldable t where
  foldr   :: (a -> b -> b) -> b -> t a -> b
  foldr   :: (a -> b -> b) -> b -> t a -> b
  foldl   :: (b -> a -> b) -> b -> t a -> b
  foldr1  :: (a -> a -> a) -> t a -> a
  foldl1  :: (a -> a -> a) -> t a -> a
  null    :: t a -> Bool
  length  :: t a -> Int
  elem    :: Eq a => a -> t a -> Bool
  maximum :: Ord a => t a -> a
  minimum :: Ord a => t a -> a
  sum     :: Num a => t a -> a
  product :: Num a => t a -> a
\end{verbatim}


Make `List` an instance of `Foldable`:

\begin{code}
instance Foldable List where
  foldr :: (a -> b -> b) -> b -> List a -> b
  foldr = undefined
\end{code}


-- Automatic derivation

The Haskell compiler can automagically derive instances of the classes Eq, Ord,
Enum, Bounded, Show, and Read for user defined data types (which meet certain
criteria) by using the `deriving` clause. This makes it easier to compare,
enumerate, print/parse data when the default behavior is sufficient.

E.g.,

\begin{code}
data Suit = Diamond | Club | Heart | Spade 
            deriving (Eq, Ord, Enum, Bounded, Show, Read)
\end{code}
