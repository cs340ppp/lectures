---
title: "Lists"
sub_title: "CS 340: Programming Patterns and Paradigms"
author: "Michael Lee <lee@iit.edu>"
---

# Agenda

- List basics
- List construction
- Infinite lists
- List comprehensions
- Common list functions
- Pattern matching with lists

---

# List basics

- Lists are the most fundamental data structure in Haskell

- A list is a *homogeneous* collection of values (all elements have the same type)

- Lists can be finite or infinite (thanks to lazy evaluation!)

<!-- pause -->

```haskell
[1, 2, 3, 4, 5] :: [Int]

['h', 'e', 'l', 'l', 'o'] :: [Char]

"hello" :: String  -- String is just [Char]!

[[1,2], [3,4], [5,6]] :: [[Int]]
```

---

# List construction

Lists are constructed using the cons operator `(:)` and the empty list `[]`

```haskell
1 : 2 : 3 : 4 : 5 : []  -- same as [1,2,3,4,5]
```

<!-- pause -->

The cons operator is right-associative:

```haskell
1 : (2 : (3 : (4 : (5 : []))))
```

<!-- pause -->

We can pattern match on lists using `:` and `[]`:

```haskell
head' :: [a] -> a
head' (x:_) = x

tail' :: [a] -> [a]
tail' (_:xs) = xs

null' :: [a] -> Bool
null' [] = True
null' _  = False
```

---

# Infinite lists

Thanks to lazy evaluation, we can define infinite lists!

```haskell
ones :: [Int]
ones = 1 : ones
```

<!-- pause -->

```haskell
repeat' :: a -> [a]
repeat' x = x : repeat' x
```

<!-- pause -->

```haskell
enumFrom' :: Enum a => a -> [a]
enumFrom' x = x : enumFrom' (succ x)
```

<!-- pause -->

```haskell
take 5 ones            --> [1,1,1,1,1]
take 3 (repeat' 'a')   --> ['a','a','a']
take 5 (enumFrom' 10)  --> [10,11,12,13,14]
```

---

# List comprehensions

List comprehensions provide a concise way to create lists:

```haskell
[x^2 | x <- [1..10]]
```

<!-- pause -->

With predicates:

```haskell
evens = [x | x <- [1..], x `mod` 2 == 0]
```

<!-- pause -->

With multiple generators:

```haskell
cartesianProduct :: [a] -> [b] -> [(a,b)]
cartesianProduct xs ys = [(x,y) | x <- xs, y <- ys]
```

<!-- pause -->

With local bindings:

```haskell
integerRightTriangles p = [(a,b,c) | a <- [1..p], 
                                     b <- [a..(p-a)],
                                     let c = p-(a+b),
                                     a^2 + b^2 == c^2]
```

---

# List comprehensions

Nested list comprehensions:

```haskell
sudokuBoxes = [[[r,c] | r <- rs, c <- cs] 
               | rs <- ["ABC", "DEF", "GHI"],
                 cs <- ["123", "456", "789"]]
```

<!-- pause -->

Flattening nested lists:

```haskell
concat' :: [[a]] -> [a]
concat' ls = [x | l <- ls, x <- l]
```

---

# Common list functions

## Length

```haskell
length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs
```

---

# Common list functions

## Last element

```haskell
last' :: [a] -> a
last' (x:[]) = x
last' (_:xs) = last' xs
```

---

# Common list functions

## Append

```haskell
(+++) :: [a] -> [a] -> [a]
[] +++ ys = ys
(x:xs) +++ ys = x : xs +++ ys
```

---

# Common list functions

## Index

```haskell
(!!!) :: [a] -> Int -> a 
(x:_) !!! 0 = x
(_:xs) !!! n = xs !!! (n-1)
```

---

# Common list functions

## Reverse

```haskell
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs +++ [x]
```

Note: This is inefficient! Can we do better?

---

# Common list functions

## Take

```haskell
take' :: Int -> [a] -> [a]
take' 0 _ = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs
```

---

# Common list functions

## Split at

```haskell
splitAt' :: Int -> [a] -> ([a], [a])
splitAt' _ [] = ([],[])
splitAt' 0 xs = ([], xs)
splitAt' n (x:xs) = let (ys,zs) = splitAt' (n-1) xs
                    in (x:ys, zs)
```

---

# Common list functions

## Break

```haskell
break' :: (a -> Bool) -> [a] -> ([a], [a])
break' _ [] = ([],[])
break' p l@(x:xs) | p x = ([], l)
                  | otherwise = let (ys, zs) = break' p xs
                                in (x:ys, zs)
```

---

# Common list functions

## Words

```haskell
words' :: String -> [String]
words' [] = []
words' l@(c:cs) | isSpace c = words' cs
                | otherwise = let (w, ws) = break' isSpace l
                              in w : words' ws
```

---

# Pattern matching with lists

Common patterns:

```haskell
-- Empty list
foo [] = ...

-- Single element
foo [x] = ...

-- At least one element
foo (x:xs) = ...

-- At least two elements
foo (x:y:xs) = ...

-- Exactly two elements
foo [x,y] = ...
```

---

# Example: Caesar cipher

```haskell
caesar :: Int -> String -> String
caesar _ [] = []
caesar n (x:xs) = (if isLetter x then encrypt x else x) 
                  : caesar n xs
  where encrypt x = n2l ((l2n x + n) `mod` 26)
        l2n c = ord (toUpper c) - ord 'A'
        n2l n = chr (n + ord 'A')
```

<!-- pause -->

```
caesar 3 "HELLO WORLD"  --> "KHOOR ZRUOG"
```
