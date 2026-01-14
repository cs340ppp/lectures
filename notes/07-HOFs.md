# Higher Order Functions

## Agenda

- HOFs and Combinators
- Basic combinators
- Recursive patterns via HOFs
- Bonus HOFs

## HOFs and Combinators

A higher-order function (HOF) is a function that takes a function as a parameter or returns a function.

(Non-HOFs are called first-order functions).

They are a fundamental tool in functional programming.

The term "combinator" is often used to refer to HOFs that combine or apply argument functions to do all their work.

## Basic combinators

### 1. Application

```haskell
($) :: (a -> b) -> a -> b
infixr 0 $
f $ x = f x
```

It seems redundant (why?), but is quite useful in practice!

#### Application: `($)`

E.g., how can we rewrite the following expressions?

```haskell
putStrLn ("Hello" ++ " " ++ "World")

show (abs (2 - 5))

take 5 (drop 10 (zip [1..] (repeat 'a')))
```

Using `$` to eliminate parentheses:

```haskell
putStrLn $ "Hello" ++ " " ++ "World"

show $ abs $ 2 - 5

take 5 $ drop 10 $ zip [1..] $ repeat 'a'
```

### 2. Composition

```haskell
(.) :: (b -> c) -> (a -> b) -> a -> c
infixr 9 .
(f . g) x = f (g x)
```

Allows us to build new functions by composing existing ones!

#### Composition: `(.)`

E.g., re-implement `even'` with composition:

```haskell
even' :: Integral a => a -> Bool
even' x = 0 == (x `rem` 2)
```

```haskell
even' :: Integral a => a -> Bool
even' = (==0) . (`rem` 2)
```

E.g., compose temperature conversion functions:

```haskell
k2c :: Num a => a -> a
k2c k = k - 273

c2f :: Fractional a => a -> a
c2f c = c * 9 / 5 + 32

f2h :: (Ord a, Num a) => a -> String
f2h f
  | f < 0     = "too cold"
  | f > 100   = "too hot"
  | otherwise = "survivable"
```

```haskell
k2h :: (Ord a, Fractional a) => a -> String
k2h = f2h . c2f . k2c
```

E.g., strip whitespace from both ends of a string:

```haskell
strip :: String -> String
strip s = reverse $ dropWhile isSpace $
          reverse $ dropWhile isSpace s
```

```haskell
strip :: String -> String
strip = reverse . dropWhile isSpace .
        reverse . dropWhile isSpace
```

### 3. Flip, On, and (many) others

Combinators are especially useful when paired with other HOFs!

```haskell
flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x
```

```haskell
on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
on g f x y = g (f x) (f y)
```

## Recursive patterns via HOFs

### 1. Map

Apply a function to each item of a list, returning the new list.

```haskell
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs
```

#### Map examples

```haskell
map (^2) [1..10]
-- [1,4,9,16,25,36,49,64,81,100]

take 10 $ map (^2) [1..]
-- [1,4,9,16,25,36,49,64,81,100]

map reverse $ words "madam I refer to adam"
-- ["madam","I","refer","ot","mada"]

map (\x -> (x,x^2)) [1..10]
-- [(1,1),(2,4),(3,9),...,(10,100)]
```

```haskell
map (++) $ words "on over under across"
-- [("on"++),("over"++),("under"++),("across"++)]

map ($ " the sea") $ map (++) $ words "on over under across"
-- ["on the sea","over the sea","under the sea","across the sea"]

map ($ "jump ") $ map (flip (++)) $ words "on over under across"
-- ["jump on","jump over","jump under","jump across"]

map (map (*2)) [[1..5], [6..10], [11..15]]
-- [[2,4,6,8,10],[12,14,16,18,20],[22,24,26,28,30]]
```

### 2. Filter

Keep only the elements of a list that satisfy a predicate.

```haskell
filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x:xs)
  | p x       = x : filter p xs
  | otherwise = filter p xs
```

#### Filter examples

```haskell
filter even [1..10]
-- [2,4,6,8,10]

take 10 $ filter even [1..]
-- [2,4,6,8,10,12,14,16,18,20]

filter (\(a,b,c) -> a^2+b^2 == c^2) $
       [(a,b,c) | a <- [1..10], b <- [a..10], c <- [b..10]]
-- [(3,4,5),(6,8,10)]

filter (\s -> reverse s == s) $
       words "madam I refer to adam"
-- ["madam","I","refer","adam"]
```

```haskell
map (\w -> (w,length w)) $
    filter (\s -> reverse s == s) $
           words "madam I refer to adam"
-- [("madam",5),("I",1),("refer",5),("adam",4)]
```

### 3. All & Any

```haskell
all :: (a -> Bool) -> [a] -> Bool
all _ [] = True
all p (x:xs) = p x && all p xs

any :: (a -> Bool) -> [a] -> Bool
any _ [] = False
any p (x:xs) = p x || any p xs
```

#### All & Any examples

```haskell
all even [2,4..10]
-- True

any even [1..]
-- True (stops as soon as it finds an even number!)

filter (any isDigit) $ words "hello 123 world a456b"
-- ["123","a456b"]
```

### 4. HOF versions of sort (and others)

```haskell
sort :: Ord a => [a] -> [a]
sort [] = []
sort (x:xs) = sort [y | y <- xs, y < x]
              ++ [x]
              ++ sort [y | y <- xs, y >= x]
```

```haskell
sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy cmp [] = []
sortBy cmp (x:xs) = sortBy cmp [y | y <- xs, cmp y x == LT]
                    ++ [x]
                    ++ sortBy cmp [y | y <- xs, cmp y x /= LT]
```

#### sortBy examples

```haskell
sortBy (flip compare) [1..10]
-- [10,9,8,7,6,5,4,3,2,1]

sortBy (compare `on` length) $ words "madam I refer to adam"
-- ["I","adam","madam","refer"]

minimumBy (compare `on` length) $ words "madam I refer to adam"
-- "I"
```

### 5. Fold

Consider the recursive patterns found in:

```haskell
and :: [Bool] -> Bool
and [] = True
and (x:xs) = (&&) x $ and xs

showCat :: Show a => [a] -> String
showCat [] = ""
showCat (x:xs) = ((++) . show) x $ showCat xs
```

What is the essential pattern here?

#### Right fold

Write the HOF that captures this pattern:

```haskell
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ v [] = v
foldr f v (x:xs) = f x (foldr f v xs)
```

E.g., trace the evaluation of `foldr (+) 0 [1..5]`:

```
foldr (+) 0 (1 : (2 : (3 : (4 : (5 : [])))))

= (+) 1 (foldr (+) 0 (2 : (3 : (4 : (5 : [])))))
= (+) 1 ((+) 2 (foldr (+) 0 (3 : (4 : (5 : [])))))
= (+) 1 ((+) 2 ((+) 3 (foldr (+) 0 (4 : (5 : [])))))
= (+) 1 ((+) 2 ((+) 3 ((+) 4 (foldr (+) 0 (5 : [])))))
= (+) 1 ((+) 2 ((+) 3 ((+) 4 ((+) 5 (foldr (+) 0 [])))))
= (+) 1 ((+) 2 ((+) 3 ((+) 4 ((+) 5 0))))
= 1 + (2 + (3 + (4 + 5)))
= 15
```

#### Right fold examples

Let's define some recursive functions in terms of foldr:

```haskell
and' :: [Bool] -> Bool
and' = foldr (&&) True

showCat' :: Show a => [a] -> String
showCat' = foldr ((++) . show) ""

(+++) :: [a] -> [a] -> [a]
l1 +++ l2 = foldr (:) l2 l1

length' :: [a] -> Int
length' = foldr (\_ n -> n+1) 0
```

```haskell
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x xs -> f x : xs) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x xs -> if p x then x:xs else xs) []
```

#### Right fold characteristics

- In what order are the input list elements evaluated?
  - Left to right (but lazily!)

- When are the combining functions applied?
  - After recursing to the end

- Does the right fold work on infinite lists?
  - Yes, if the combining function is lazy in its second argument!

#### Left fold

Consider the recursive patterns found in:

```haskell
hash :: Integer -> String -> Integer
hash seed [] = seed
hash seed (c:cs) = hash (h seed c) cs
  where h v c = (7*v `xor` fromIntegral (ord c)) `mod` 1000007
```

How is the pattern different from before?

The accumulator is passed as a parameter and updated on each recursive call!

Write the HOF that captures this pattern:

```haskell
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ v [] = v
foldl f v (x:xs) = foldl f (f v x) xs
```

E.g., trace the evaluation of `foldl (+) 0 [1..5]`:

```
foldl (+) 0 (1 : (2 : (3 : (4 : (5 : [])))))

= foldl (+) ((+) 0 1) (2 : (3 : (4 : (5 : []))))
= foldl (+) ((+) ((+) 0 1) 2) (3 : (4 : (5 : [])))
= foldl (+) ((+) ((+) ((+) 0 1) 2) 3) (4 : (5 : []))
= foldl (+) ((+) ((+) ((+) ((+) 0 1) 2) 3) 4) (5 : [])
= foldl (+) ((+) ((+) ((+) ((+) ((+) 0 1) 2) 3) 4) 5) []
= ((+) ((+) ((+) ((+) ((+) 0 1) 2) 3) 4) 5)
= ((((0 + 1) + 2) + 3) + 4) + 5
= 15
```

#### Left fold examples

Let's define some recursive functions in terms of foldl:

```haskell
hash' :: String -> Integer
hash' = foldl h 0
  where h v c = (7*v `xor` fromIntegral (ord c)) `mod` 1000007

playMoves' :: [(Int,Char)] -> [Char]
playMoves' = foldl move "         "
  where move board (x,y) = take x board ++ [y] ++ drop (x+1) board
```

#### Left fold characteristics

- In what order are the input list elements evaluated?
  - Left to right

- When is the combining function applied?
  - Immediately, before recursing

- Does the left fold work on infinite lists?
  - No! It needs to reach the end of the list

- How might we make the left fold more efficient?
  - Use strict evaluation with `foldl'` from `Data.List`

#### When to fold left or right?

- Use `foldr` when:
  - Working with infinite lists
  - The combining function is lazy in its second argument
  - Building a data structure (like a list)

- Use `foldl` (or better, `foldl'`) when:
  - Computing a single result (like a sum or product)
  - The combining function is strict
  - Working with finite lists

## Bonus HOFs

### foldr1 and foldl1

Folds that use the first element of the list as the initial value:

```haskell
foldr1 :: (a -> a -> a) -> [a] -> a
foldr1 _ [x] = x
foldr1 f (x:xs) = f x (foldr1 f xs)

foldl1 :: (a -> a -> a) -> [a] -> a
foldl1 f (x:xs) = foldl f x xs
```

```haskell
foldr1 (*) [1..5]  -- 120
foldr1 (^) [2,2,3]  -- 256

foldl1 (++) [[1,2], [3,4], [5,6]]  -- [1,2,3,4,5,6]
foldl1 (/) [16,2,4]  -- 2.0
```

### iterate

Takes a function and an initial value, and returns an infinite list of repeated applications of the function to the initial value.

```haskell
iterate :: (a -> a) -> a -> [a]
iterate f x = x : iterate f (f x)
```

```haskell
take 10 $ iterate (*2) 1
-- [1,2,4,8,16,32,64,128,256,512]
```

### until

Takes a predicate and a function, and returns the first value that satisfies the predicate when repeatedly applied to the function.

```haskell
until :: (a -> Bool) -> (a -> a) -> a -> a
until p f x
  | p x       = x
  | otherwise = until p f (f x)
```

```haskell
until (> 100) (*2) 1
-- 128

let n = 2
in until (\x -> abs (n-x*x) < 0.001)
         (\x -> (x + n/x) / 2)
         1.0
-- 1.414215686274509 (approximately sqrt(2))
```

## Summary

Higher-order functions allow us to:

- Abstract common patterns of recursion
- Write more concise and expressive code
- Compose simple functions into complex ones
- Separate *what* we want to do from *how* we do it

Key HOFs to remember:

- `map`, `filter`, `foldr`, `foldl`
- Combinators: `$`, `.`, `flip`, `on`
