% CS 340: Programming Paradigms and Patterns
% Lect 04 - Lists
% Michael Lee

> module Lect04 where
> import Data.Char

Lists
=====

Agenda:
  - The List type
  - Syntactic sugar
  - Constructing lists
  - List comprehensions
  - Common list functions
  - List processing functions
    - Pattern matching
    - Structural recursion


The List type
-------------

Haskell's built-in list type might be defined something like this:

    [a] = [] | a : [a]

Read as: a list of type 'a' ([a]) is either an empty list ([]) or 
         value of type 'a' followed by ':' and a list of type 'a' ([a]).

Takeaways: 
  - a list is defined recursively, i.e., in terms of itself
  - the list type is polymorphic
  - lists are homogeneous (all elements of a given list are of the same type)


`[]` and `:` are examples of value constructors (aka data constructors)

  - we call `:` the "cons" operator -- it is right-associative, and has type:

    (:) :: a -> [a] -> [a]

Try using `[]` and `:` to build some lists.


Syntactic sugar
---------------

Instead of constructing lists with `:`, there is syntactic sugar:

E.g., for simple itemized lists, [...]

    [1,2,3,4,5,6,7,8,9,10]   ==  1:2:3:4:5:6:7:8:9:10:[] 

E.g., for lists of characters (string), "...":

    "hello world"       ==  'h':'e':'l':'l':'o':[]

E.g., for instances of `Enum`, [I..J] and [I,J..K] and [I..]; try:

    [1..10]

    ['a'..'z']

    [2,4..10]
    
    [10,9..1]


E.g., for infinite lists of `Enum`, [I..] and [I,J..]

    [1..]    ==  enumFrom 1

    [3,6..]  ==  enumFromThen 3 6


Constructing lists
------------------

Functions that construct lists typically:

  - operate recursively

  - add just one element with `:`, when necessary, and recurse to construct 
    the rest of the list

  - terminate the list in the base case with `[]`


E.g., implement the following list construction functions:

> replicate' :: Int -> a -> [a]
> replicate' = undefined
>
> enumFromTo' :: (Ord a, Enum a) => a -> a -> [a]
> enumFromTo' = undefined
>
> -- and now for some infinite lists
>
> ones :: [Int]
> ones = undefined
> 
> repeat' :: a -> [a]
> repeat' = undefined
>
> enumFrom' :: Enum a => a -> [a]
> enumFrom' = undefined


Note: use `take` to limit the number of values drawn from an infinite list


List comprehensions
-------------------

Syntax:

  [ Expression | Generator, ... , Predicate, ... ]

  - which produces a list of values computed by `Expression`

  - where each `Generator` is of the form "var <- List"

  - and each `Predicate` is a Boolean expression

  - you can also use `let` to create local vars (without `in`)

E.g.,

> evens = [2*x | x <- [1..]]
>
> evens' = [x | x <- [1..], x `mod` 2 == 0]
>
> integerRightTriangles p = [(a,b,c) | a <- [1..p], 
>                                      b <- [a..(p-a)],
>                                      let c = p-(a+b),
>                                      a^2 + b^2 == c^2]

E.g., try implementing:

> factors :: Integral a => a -> [a]
> factors = undefined
>
> cartesianProduct :: [a] -> [b] -> [(a,b)]
> cartesianProduct = undefined
>
> concat' :: [[a]] -> [a]
> concat' = undefined


Common list functions
---------------------

The "Prelude" module defines many useful list functions (some of which we 
implemented above). They include:

  - Basic operations:

    head :: [a] -> a
    tail :: [a] -> [a]
    null :: [a] -> Bool
    length :: [a] -> Int
    last :: [a] -> a
    (++) :: [a] -> [a] -> [a]
    (!!) :: [a] -> Int -> a

  - Building lists:

    repeat :: a -> [a]
    replicate :: Int -> a -> [a]
    cycle :: [a] -> [a]

  - Lists -> Lists:

    concat :: [[a]] -> [a]
    reverse :: [a] -> [a]
    zip :: [a] -> [b] -> [(a,b)]

  - Extracting sublists:

    take :: Int -> [a] -> [a]
    drop :: Int -> [a] -> [a]
    splitAt :: Int -> [a] -> ([a], [a])
    break :: (a -> Bool) -> [a] -> ([a], [a])

  - Class specific:

    elem :: Eq a => a -> [a] -> Bool
    maximum :: Ord a => [a] -> a
    minimum :: Ord a => [a] -> a
    sum :: Num a => [a] -> a
    product :: Num a => [a] -> a
    lines :: String -> [String]
    words :: String -> [String]

Note: many of these functions operate on a type class that includes lists and
      other recursive data types (We'll see how this works later.)


List processing functions
-------------------------

-- Pattern matching

`[]` and `:` can be used to pattern match against lists (we'll see that all
value constructors can be used for pattern matching). 

E.g., implement:

> head' :: [a] -> a
> head' = undefined
>
> tail' :: [a] -> [a]
> tail' = undefined
> 
> null' :: [a] -> Bool
> null' = undefined


-- Structural recursion

Structural recursion describes a pattern for writing functions that process recursively defined data types (like the list). 

Generally, a structurally recursive function will:

  1. start by determining how the value was constructed

  2. if the value is not a recursive instance of the data type, (e.g., `[]`) 
     just process it directly

  3. if the value is a recursive instance of the data type, "deconstruct" it to 
     process one value, then recurse to process the rest of the values.

Pattern matching in Haskell helps with both (1) and (3).

E.g., to compute the length of a list:

> length' :: [a] -> Int
> length' [] = 0
> length' (x:xs) = 1 + length' xs


E.g., implement more built-in functions:

> last' :: [a] -> a
> last' = undefined
>
>
> (+++) :: [a] -> [a] -> [a]
> (+++) = undefined
>
>
> (!!!) :: [a] -> Int -> a -- the ! in its name is an implicit warning as to its inefficiency!
> (!!!) = undefined
>
>
> reverse' :: [a] -> [a]
> reverse' = undefined
>
>
> take' :: Int -> [a] -> [a]
> take' = undefined
>
>
> splitAt' :: Int -> [a] -> ([a], [a])
> splitAt' = undefined
>
>
> break' :: (a -> Bool) -> [a] -> ([a], [a])
> break' = undefined
>
>
> words' :: String -> [String]
> words' = undefined


E.g., the Caesar cipher is an encryption scheme that takes a plain text input
string P and a shift value N, and produces an encrypted version of the string
by replacing each letter in P with one N letters away in the alphabet (wrapping
around, if needed).

For the string "HELLO WORLD" and a shift value of 5:

  Plain:      H E L L O  W O R L D
  Encrypted:  M J Q Q T  B T W Q I

To implement the Caesar cipher, we need to be able to convert characters from/to
their corresponding ASCII codes. `ord`/`chr` do this. `isLetter` can be used to
determine if a character is a letter. We'll convert all letters to uppercase
for simplicity with `toUpper`.

> caesar :: Int -> String -> String
> caesar = undefined
