---
title: "Recursion"
sub_title: "CS 340: Programming Patterns and Paradigms"
author: "Michael Lee <lee@iit.edu>"
---

# Agenda

- Common patterns of recursion:
  - Iteration & Reduction
  - Filtering
  - Accumulation
  - Combinations & Permutations
  - Divide & Conquer
  - Generative recursion

---

# Recursion

Recursion is the fundamental mechanism for iteration in functional programming.

- A recursive function is one that calls itself

- Every recursive function needs:
  - *Base case(s)*: conditions where recursion stops
  - *Recursive case(s)*: where the function calls itself with "smaller" inputs

---

# A. Iteration & Reduction

Processing elements one by one

---

# A. Iteration & Reduction

## Sum up the elements of a list

```haskell
sum' :: Num a => [a] -> a
sum' [] = 0                    -- base case
sum' (x:xs) = x + sum' xs      -- recursive case
```

<!-- pause -->

E.g., trace the evaluation of `sum' [1,2,3]`:

```
sum' [1,2,3]
= 1 + sum' [2,3]
= 1 + (2 + sum' [3])
= 1 + (2 + (3 + sum' []))
= 1 + (2 + (3 + 0))
= 1 + (2 + 3)
= 1 + 5
= 6
```

---

# A. Iteration & Reduction

## A classic!

```haskell
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n-1)
```

<!-- pause -->

E.g., trace the evaluation of `factorial 5`:

```
factorial 5
= 5 * factorial 4
= 5 * (4 * factorial 3)
= 5 * (4 * (3 * factorial 2))
= 5 * (4 * (3 * (2 * factorial 1)))
= 5 * (4 * (3 * (2 * (1 * factorial 0))))
= 5 * (4 * (3 * (2 * (1 * 1))))
= 120
```

---

# B. Filtering

Selective iteration/reduction

---

# B. Filtering

## Sum only the positive numbers in a list

```haskell
sumPositives :: Integral a => [a] -> a
sumPositives [] = 0
sumPositives (x:xs) 
  | x > 0     = x + sumPositives xs
  | otherwise = sumPositives xs
```

---

# B. Filtering

## Palindromes are strings that read the same forwards as backwards

```haskell
palindromes :: [String] -> [String]
palindromes [] = []
palindromes (s:ss)
  | s == reverse s = s : palindromes ss
  | otherwise      = palindromes ss
```

---

# C. Accumulation

Computing/Passing information "down" while recursing

---

# C. Accumulation

## Reverse a list

```haskell
reverse' :: [a] -> [a]
reverse' xs = rev xs []
  where rev [] acc     = acc
        rev (x:xs) acc = rev xs (x:acc)
```

<!-- pause -->

E.g., trace the evaluation of `reverse' [1,2,3]`:

```
reverse' [1,2,3]
= rev [1,2,3] []
= rev [2,3] [1]
= rev [3] [2,1]
= rev [] [3,2,1]
= [3,2,1]
```

---

# D. Combinations & Permutations

Essential combinatorics

---

# D. Combinations & Permutations

## Generate all combinations of elements in a list

Order doesn't matter:

```haskell
combinations :: [a] -> [[a]]
combinations [] = [[]]
combinations (x:xs) = combinations xs 
                      ++ map (x:) (combinations xs)
```

<!-- pause -->

E.g., `combinations [1,2,3]` produces:

```
[[],[3],[2],[2,3],[1],[1,3],[1,2],[1,2,3]]
```

---

# D. Combinations & Permutations

## Knapsack problem

Given a list of items (value,weight) and a weight capacity, find the maximum 
value that can be carried:

```haskell
knapsack :: (Ord a, Num a) => a -> [(a,a)] -> a
knapsack _ [] = 0
knapsack cap ((v,w):items)
  | w > cap   = knapsack cap items
  | otherwise = max (knapsack cap items)
                    (v + knapsack (cap-w) items)
```

<!-- pause -->

E.g., `knapsack 10 [(60,6), (90,8), (50,2), (40,2)]` = 150

---

# D. Combinations & Permutations

## Generate all permutations of elements in a list

Order matters:

```haskell
permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations xs = [x:ps | x <- xs, 
                          ps <- permutations (delete x xs)]
  where delete y [] = []
        delete y (z:zs) | y == z    = zs
                        | otherwise = z : delete y zs
```

<!-- pause -->

E.g., `permutations [1,2,3]` produces:

```
[[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]
```

---

# D. Combinations & Permutations

## Generate all palindromes from a given string

```haskell
allPalindromes :: String -> [String]
allPalindromes s = nub [p | p <- permutations s, 
                            p == reverse p]
```

<!-- pause -->

E.g., `allPalindromes "aab"` produces:

```
["aba","baa"]  -- wait, "baa" isn't a palindrome!
```

Actually produces just `["aba"]` when `nub` removes duplicates.

---

# E. Divide & Conquer

Break a problem into smaller ones of the same structure

---

# E. Divide & Conquer

## Another classic!

```haskell
fib :: Int -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
```

<!-- pause -->

This is elegant but *very* inefficient! We recompute the same values many times.

<!-- pause -->

Solution: memoization (caching results)

---

# E. Divide & Conquer

## Sort by splitting the list in half and merging the sorted halves

```haskell
mergesort :: Ord a => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort xs = merge (mergesort left) (mergesort right)
  where (left, right) = splitAt (length xs `div` 2) xs
        merge [] ys = ys
        merge xs [] = xs
        merge (x:xs) (y:ys)
          | x <= y    = x : merge xs (y:ys)
          | otherwise = y : merge (x:xs) ys
```

---

# F. Generative recursion

Generates new subproblems (in size/structure)

---

# F. Generative recursion

## Newton's method for finding square roots

1. Start with a guess g -- for sqrt x, try g=x/2
2. Is g^2 = x?
   - if so, we're done
3. Improve the guess; g'=(g + x/g)/2
   (if g is too low this will increase it, and vice versa)

```haskell
sqrt' :: Double -> Double
sqrt' x = improve (x/2)
  where improve g 
          | abs (g*g - x) < 0.0001 = g
          | otherwise = improve ((g + x/g) / 2)
```

<!-- pause -->

E.g., `sqrt' 16` converges quickly to 4.0

---

# Summary

Common recursion patterns:

- **Iteration & Reduction**: processing elements one by one
- **Filtering**: selective processing based on predicates
- **Accumulation**: building results with accumulators
- **Combinations & Permutations**: exploring all possibilities
- **Divide & Conquer**: splitting problems into smaller subproblems
- **Generative recursion**: creating new subproblems dynamically
