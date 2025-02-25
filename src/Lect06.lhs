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
-- reverse a list
reverse' :: [a] -> [a]
reverse' = undefined
\end{code}


D. Combinations & Permutations (Essential combinatorics)

\begin{code}
-- generate all combinations of elements in a list (order doesn't matter)
combinations :: [a] -> [[a]]
combinations = undefined

-- knapsack problem: given a list of items (value,weight) and a weight 
--                   capacity, find the maximum value that can be carried
-- e.g., knapsack 10 [(60,6), (90,8), (50,2), (40,2)] = 150
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
-- another classic!
fib :: Int -> Integer
fib = undefined

-- sort by splitting the list in half and merging the sorted halves
mergesort :: Ord a => [a] -> [a]
mergesort = undefined
\end{code}


F. Generative recursion (Generates new subproblems (in size/structure))

\begin{code}
-- Newton's method for finding square roots
-- 1. Start with a guess g -- for sqrt x, try g=x/2
-- 2. Is g^2 = x?
--    - if so, we're done
-- 3. Improve the guess; g'=(g + x/g)/2
--    (if g is too low this will increase it, and vice versa)
sqrt' :: Double -> Double
sqrt' = undefined
\end{code}
