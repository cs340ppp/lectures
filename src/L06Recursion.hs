module L06Recursion where
import Data.List (nub)
import Debug.Trace
import Data.Function.Memoize

-- sum up the elements of a list
sum' :: Num a => [a] -> a
sum' = undefined

-- a classic!
factorial :: Integer -> Integer
factorial = undefined

-- sum only the positive numbers in a list
sumPositives :: Integral a => [a] -> a
sumPositives = undefined

-- palindroms are strings that read the same forwards as backwards
palindromes :: [String] -> [String]
palindromes = undefined

-- reverse a list
reverse' :: [a] -> [a]
reverse' = undefined

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

-- another classic!
fib :: Int -> Integer
fib = undefined

-- sort by splitting the list in half and merging the sorted halves
mergesort :: Ord a => [a] -> [a]
mergesort = undefined

-- Newton's method for finding square roots
-- 1. Start with a guess g -- for sqrt x, try g=x/2
-- 2. Is g^2 = x?
--    - if so, we're done
-- 3. Improve the guess; g'=(g + x/g)/2
--    (if g is too low this will increase it, and vice versa)
sqrt' :: Double -> Double
sqrt' = undefined
