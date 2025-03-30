module Lect01 where
import Prelude hiding (Functor)
import Data.List

quicksort :: Ord a => [a] -> [a]
quicksort []     = []
quicksort (x:xs) = quicksort lesser ++ [x] ++ quicksort larger
    where lesser = filter (< x)  xs
          larger = filter (>= x) xs

foo :: Int -> Int
foo x = 2 * x

inflist :: [Int]
inflist = gen 1
  where gen x = x : gen (x+1) -- infinite recursion
