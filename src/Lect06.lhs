% CS 340: Programming Paradigms and Patterns
% Lect 06 - Recursion
% Michael Lee

\begin{code}
module Lect06 where
import Debug.Trace
import qualified Data.Set as Set
\end{code}

Recursion
=========

Agenda:

  - Some common patterns of recursion:
     A. Iteration & Reduction
     B. Filtering
     C. Combinations & Permutations
     D. Divide & Conquer
     E. Tail recursion & Accumulation
  - How to trace and debug in Haskell
  - How laziness affects recursive call evaluation


A. Iteration & Reduction

Iteration is the process of repeatedly applying a function to a value
until one or more conditions (base cases) are met. It often makes sense to 
think of iteration as incrementally "building up" a result, as in constructing 
a list element by element. Sometimes, iteration is used to "reduce" an input to a final value (e.g., as in summing up the elements of a list).

E.g., implement the following functions using iteration/reduction:

\begin{code}
-- a classic!
factorial :: Integer -> Integer
factorial = undefined


-- sum up the elements of a list
sumList :: (Show a, Num a) => [a] -> a
sumList = undefined


-- sometimes we iterate over lists in parallel
weightedSum :: (Show a, Num a) => [a] -> [a] -> a
weightedSum = undefined


-- sometimes we process more than one "item" at a time
swapLetters :: String -> String
swapLetters = undefined


-- implement this using append (++)
cycle' :: [a] -> [a]
cycle' = undefined


-- can we do better? (why is it better?)
cycle'' :: [a] -> [a]
cycle'' = undefined


-- we'll need to pass values into subsequent iterations to track progress
fibs :: [Integer]
fibs = undefined
\end{code}


B. Filtering (conditional iteration/reduction)

Filtering is the process of iterating over a list and processing only those elements that satisfy a given condition. 

\begin{code}
-- sum only the positive numbers in a list
sumPositives :: Integral a => [a] -> a
sumPositives = undefined


-- palindroms are strings that read the same forwards as backwards
palindromes :: [String] -> [String]
palindromes = undefined
\end{code}


C. Combinations & Permutations

Combinations and permutations are classic problems in combinatorics that arise 
in many different problems.

\begin{code}
-- generate all combinations (order doesn't matter -- how many are there?)
combinations :: [a] -> [[a]]
combinations = undefined


-- generate all combinations of a given size (nCr = n!/(r!(n-r)!))
combinations' :: Int -> [a] -> [[a]]
combinations' = undefined


-- the "making change" problem
change :: (Ord a, Num a) => a -> [a] -> [[a]]
change = undefined


-- the knapsack problem: given a list of items (value,weight) and a weight 
-- capacity, find the maximum value that can be carried
knapsack :: (Ord a, Num a) => a -> [(a,a)] -> a
knapsack = undefined


-- find the actual set of items that maximizes value (under the weight cap)
knapsack' :: (Ord a, Num a) => a -> [(a,a)] -> [(a,a)]
knapsack' = undefined


-- find the two closest points in a list of points (brute force)
closestPoints :: (Ord a, Num a) => [(a,a)] -> [(a,a)]
closestPoints = undefined


-- generate all permutations (order matters -- how many are there?)
permutations :: [a] -> [[a]]
permutations = undefined


-- generate all palindromes from a given string
allPalindromes :: String -> [String]
allPalindromes = undefined
\end{code}


D. Divide & Conquer

Divide and conquer is a technique for solving problems by breaking them into
smaller subproblems and then combining the solutions to the subproblems to
obtain a solution to the original problem.

\begin{code}
-- a classic!
fib :: Integral a => a -> a
fib = undefined


-- sort by splitting the list in half and merging the sorted halves
mergesort :: Ord a => [a] -> [a]
mergesort = undefined


-- sort by choosing a pivot and "partitioning" the list around it
quicksort :: Ord a => [a] -> [a]
quicksort = undefined


-- find the two closest points in a list of points (more efficiently)
closestPoints' :: (Ord a, Num a) => [(a,a)] -> [(a,a)]
closestPoints' = undefined
\end{code}


E. Tail recursion & Accumulation

Tail recursion is a special case of recursion where the recursive call is the
last thing done in the function.  In non-lazy languages, this is important
because it allows the compiler to optimize the code by eliminating the need
for a stack frame. In Haskell (and other lazy languages), tail recursion does 
not quite have the same importance, but it is still a useful technique.

Accumulation is a technique for solving problems by passing an extra
parameter to the recursive call that accumulates the solution.

\begin{code}
-- are all elements even?
allEven :: [Integer] -> Bool
allEven = undefined


-- are two lists the same length?
sameLength :: [a] -> [b] -> Bool
sameLength = undefined


-- tail recursive factorial with explicit accumulator
factorial' :: Integer -> Integer -> Integer
factorial' = undefined


-- tail recursive factorial with hidden accumulator
factorial'' :: Integer -> Integer
factorial'' = undefined


-- reverse a list using an accumulator
reverse' :: [a] -> [a]
reverse' = undefined


-- enumerate the integers from m to n (with an accumulator)
enumFromTo' :: Integer -> Integer -> [Integer]
enumFromTo' = undefined


-- can we write the infinite list version using an accumulator?
enumFrom' :: Integer -> [Integer]
enumFrom' = undefined
\end{code}
