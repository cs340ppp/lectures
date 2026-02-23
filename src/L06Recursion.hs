module L06Recursion where
import Prelude hiding (gcd, sqrt)

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n-1)

digits :: Int -> [Int]
digits n
  | n < 10    = [n]
  | otherwise = digits (n `div` 10)
                ++ [n `mod` 10]

digits' :: Int -> [Int]
digits' 0 = [0]
digits' n = go [] n
  where go acc 0 = acc
        go acc n = go (n `mod` 10 : acc)
                      (n `div` 10)

combinations :: [a] -> [[a]]
combinations [] = [[]]
combinations (x:xs) = combinations xs
                      ++ map (x:) (combinations xs)

knapsack :: (Ord a, Num a) => a -> [(a,a)] -> a
knapsack _ [] = 0
knapsack cap ((v,w):items)
  | w > cap   = knapsack cap items
  | otherwise = max (knapsack cap items)
                    (v + knapsack (cap-w) items)

permutations :: Eq a => [a] -> [[a]]
permutations [] = [[]]
permutations xs = [x:ps | x <- xs,
                          ps <- permutations (delete x xs)]
  where delete y [] = []
        delete y (z:zs) | y == z    = zs
                        | otherwise = z : delete y zs

fib :: Int -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibMemo :: Int -> Integer
fibMemo n = memo !! n
  where memo = map fib [0..]
        fib 0 = 0
        fib 1 = 1
        fib k = memo !! (k-1) + memo !! (k-2)

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

gcd :: Integer -> Integer -> Integer
gcd a 0 = a
gcd a b = gcd b (a `mod` b)

sqrt :: Double -> Double
sqrt x = improve (x/2)
  where improve g
          | abs (g*g - x) < 0.0001 = g
          | otherwise = improve ((g + x/g) / 2)

