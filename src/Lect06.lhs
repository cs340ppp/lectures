% CS 340: Programming Paradigms and Patterns
% Lect 06 - Recursion
% Michael Lee

\begin{code}
module Lect06 where
import Data.List (nub)
import Debug.Trace
import Data.Function.Memoize
\end{code}

Recursion
=========

Agenda:

- Common patterns of recursion:
  A. Iteration & Reduction
  B. Filtering
  C. Accumulation
  D. Combinations & Permutations
  E. Divide & Conquer
  F. Generative recursion


A. Iteration & Reduction (Processing elements one by one)

\begin{code}
-- sum up the elements of a list
sum' :: Num a => [a] -> a
sum' = undefined

-- a classic!
factorial :: Integer -> Integer
factorial = undefined
\end{code}


B. Filtering (Selective iteration/reduction)

\begin{code}
-- sum only the positive numbers in a list
sumPositives :: Integral a => [a] -> a
sumPositives = undefined

-- palindroms are strings that read the same forwards as backwards
palindromes :: [String] -> [String]
palindromes = undefined
\end{code}


C. Accumulation (Computing/Passing information "down" while recursing)

\begin{code}
-- count even numbers
countEvens :: Integral a => [a] -> Int
countEvens = undefined

-- reverse a list
reverse' :: [a] -> [a]
reverse' = undefined
\end{code}


D. Combinations & Permutations (Essential combinatorics)

\begin{code}
-- generate all combinations of elements in a list (order doesn't matter)
combinations :: [a] -> [[a]]
combinations = undefined

-- the knapsack problem: given a list of items (value,weight) and a weight 
-- capacity, find the maximum value that can be carried
knapsack :: (Ord a, Num a) => a -> [(a,a)] -> a
knapsack = undefined

-- generate all permutations of elements in a list (order matters)
permutations :: [a] -> [[a]]
permutations = undefined

-- generate all palindromes from a given string (use `nub` to remove dups)
allPalindromes :: String -> [String]
allPalindromes = undefined
\end{code}


E. Divide & Conquer (Break a problem into smaller ones of the same structure)

\begin{code}
-- a classic!
fib :: Integral a => a -> a
fib = undefined

-- sort by splitting the list in half and merging the sorted halves
mergesort :: Ord a => [a] -> [a]
mergesort = undefined
\end{code}


F. Generative recursion (Generates new subproblems (in size/structure))

\begin{code}
newtonSqrt :: Double -> Double -> Double
newtonSqrt = undefined
\end{code}
