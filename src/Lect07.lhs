% CS 340: Programming Paradigms and Patterns
% Lect 07 - Higher order functions
% Michael Lee

\begin{code}
module Lect07 where
import Prelude hiding (($), (.), flip, on, 
                       map, filter, any, all, iterate, until,
                       foldr, foldl, foldr1, foldl1)
import Data.Char
import Data.Bits ( Bits(xor) )
import Data.Function hiding (($), (.), flip, on)
import Data.List (minimumBy)
import Debug.Trace
\end{code}

Higher order functions
======================

Agenda:
  - HOFs and Combinators
  - Basic combinators
  - Recursive patterns via HOFs
  - Bonus HOFs


HOFs and Combinators
--------------------

A higher-order function (HOF) is a function that takes a function as a parameter or returns a function. (Non-HOFs are called first-order functions).
They are a fundamental tool in functional programming.

The term "combinator" is often used to refer to HOFs that combine or apply argument functions to do all their work.


Basic combinators
-----------------

1. Application:

\begin{code}
($) :: (a -> b) -> a -> b
infixr 0 $
($) = undefined
\end{code}

It seems redundant (why?), but is quite useful in practice!

E.g., how can we rewrite the following expresions?

\begin{verbatim}
  putStrLn ("Hello" ++ " " ++ "World")

  show (abs (2 - 5))

  take 5 (drop 10 (zip [1..] (repeat 'a')))
\end{verbatim}


2. Composition

\begin{code}
(.) :: (b -> c) -> (a -> b) -> a -> c
infixr 9 .
(.) = undefined
\end{code}    

E.g., re-implement `even'`, `k2h`, and `strip` with composition:

\begin{code}
even' :: Integral a => a -> Bool
even' x = 0 == (x `rem` 2)


k2c :: Num a => a -> a
k2c k = k - 273

c2f :: Fractional a => a -> a
c2f c = c * 9 / 5 + 32

f2h :: (Ord a, Num a) => a -> String
f2h f
  | f < 0     = "too cold"
  | f > 100   = "too hot"
  | otherwise = "survivable"

k2h :: (Ord a, Fractional a) => a -> String
k2h  k = f2h $ c2f $ k2c k


strip :: String -> String
strip s = reverse $ dropWhile isSpace $ reverse $ dropWhile isSpace s
\end{code}


3. Flip, On, and (many) others

Combinators are especially useful when paired with other HOFs!

\begin{code}
flip :: (a -> b -> c) -> b -> a -> c
flip = undefined


on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
on = undefined
\end{code}


Recursive patterns via HOFs
---------------------------

1. Map: apply a function to each item of a list, returning the new list.

\begin{code}
map :: (a -> b) -> [a] -> [b]
map = undefined
\end{code}


E.g.,

\begin{verbatim}
  map (^2) [1..10]

  take 10 $ map (^2) [1..]

  map reverse $ words "madam I refer to adam"

  map (\x -> (x,x^2)) [1..10]

  map (++) $ words "on over under across"

  map ($ " the sea") $ map (++) $ words "on over under across"

  map ($ "jump ") $ map (flip (++)) $ words "on over under across"

  map (map (*2)) [[1..5], [6..10], [11..15]]
\end{verbatim}



2. Filter: keep only the elements of a list that satisfy a predicate.

\begin{code}
filter :: (a -> Bool) -> [a] -> [a]
filter = undefined
\end{code}                 


E.g.,

\begin{verbatim}
  filter even [1..10]

  take 10 $ filter even [1..]

  filter (\(a,b,c) -> a^2+b^2 == c^2) $
         [(a,b,c) | a <- [1..10], b <- [a..10], c <- [b..10]]

  filter (\s -> reverse s == s) $ 
         words "madam I refer to adam"

  map (\w -> (w,length w)) $ 
      filter (\s -> reverse s == s) $ 
             words "madam I refer to adam"
\end{verbatim}  


3. All & Any

\begin{code}
all :: (a -> Bool) -> [a] -> Bool
all = undefined

any :: (a -> Bool) -> [a] -> Bool
any = undefined
\end{code}

E.g.,

\begin{verbatim}
  all even [2,4..10]

  any even [1..]

  filter (any isDigit) $ words "hello 123 world a456b"
\end{verbatim}  


4. HOF versions of sort (and others)

\begin{code}
sort :: Ord a => [a] -> [a]
sort [] = []
sort (x:xs) = sort [y | y <- xs, y < x] 
              ++ [x] 
              ++ sort [y | y <- xs, y >= x]


sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy = undefined
\end{code}

E.g.,
\begin{verbatim}
  sortBy (flip compare) [1..10]

  sortBy (compare `on` length) $ words "madam I refer to adam"

  minimumBy (compare `on` length) $ words "madam I refer to adam"
\end{verbatim}  


5. Fold

Consider the recursive patterns found in:

\begin{code}
add :: (Num a) => [a] -> a
add [] = 0
add (x:xs) = (+) x $ product xs


hash :: String -> Integer
hash [] = 1307
hash (c:cs) = h c $ hash cs
  where h c r = fromIntegral (ord c) `xor` 7*r
\end{code}


What is the essential pattern here?

Write the HOF that captures this pattern:

\begin{code}
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr = undefined
\end{code}


E.g., trace the evaluation of `foldr (+) 0 [1..5]`:

  foldr (+) 0 (1 : (2 : (3 : (4 : (5 : [])))))

= ?


Let's define some recursive functions in terms of foldr:

\begin{code}
add' :: (Num a) => [a] -> a
add' = undefined


hash' :: String -> Integer
hash' = undefined


(+++) :: [a] -> [a] -> [a]
l1 +++ l2 = undefined


length' :: [a] -> Int
length' = undefined


map' :: (a -> b) -> [a] -> [b]
map' f = undefined


filter' :: (a -> Bool) -> [a] -> [a]
filter' p = undefined
\end{code}


Consider the recursive patterns found in:

\begin{code}
pow :: (Integral a) => a -> [a] -> a
pow r [] = r
pow r (x:xs) = pow (r ^ x) xs


concatShow :: (Show a) => String -> [a] -> String
concatShow r [] = r
concatShow r (x:xs) = concatShow (r ++ show x) xs
\end{code}


How is the pattern different from before?

Write the HOF that captures this pattern:

\begin{code}
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl = undefined
\end{code}


E.g., trace the evaluation of `foldl (+) 0 [1..5]`:

  foldl (+) 0 (1 : (2 : (3 : (4 : (5 : [])))))

= 

Let's define some recursive functions in terms of foldr:

\begin{code}
pow' :: (Integral a) => [a] -> a
pow' = undefined

concatShow' :: (Show a) => [a] -> String
concatShow' = undefined

reverse' :: [a] -> [a]
reverse' = undefined
\end{code}


In what order do foldr and foldl evaluate their constituent functions? (The answer may surprise you!)

Consider:

\begin{code}
foldrD :: Show a => (a -> b -> b) -> b -> [a] -> b
foldrD _ v [] = v
foldrD f v (x:xs) = traceShow x $ f x $ foldrD f v xs

foldlD :: Show a => (b -> a -> b) -> b -> [a] -> b
foldlD _ v [] = v
foldlD f v (x:xs) = let e = traceShow x (f v x) 
                    in foldlD f e xs
\end{code}


We can force Haskell to be stricter by using `seq`, which has type:

    seq :: a -> b -> b

`seq` takes two arguments and forces strict evaluation of its first argument
before evaluating the second argument (and returning a result). 


Write a strict version of `foldl`:

\begin{code}
foldlDS :: Show a => (b -> a -> b) -> b -> [a] -> b
foldlDS = undefined
\end{code}


Right folds can work with infinite lists; left folds can not! (Why?)


Bonus HOFs
----------

It is convenient to have folds that use the first element of the list as the
initial value (and thus don't require an initial value to be passed in).

E.g.,

\begin{code}
foldr1 :: (a -> a -> a) -> [a] -> a
foldr1 f (x:xs) = foldr f x xs

-- e.g., foldr1 (*) [1..5]

foldl1 :: (a -> a -> a) -> [a] -> a
foldl1 f (x:xs) = foldl f x xs

-- e.g., foldl1 (++) [[1,2], [3,4], [5,6]]
\end{code}


The `scan` variants are like fold, but they return a list of the intermediate
values of the result/accumulator.

E.g., try:

\begin{verbatim}
  scanr (+) 0 [1..5]

  scanl (+) 0 [1..5]

  scanr1 (*) [1..5]

  scanl1 (++) [[1,2], [3,4], [5,6]]
\end{verbatim}


`iterate` takes a function and an initial value, and returns an infinite list
of repeated applications of the function to the initial value.

\begin{code}
iterate :: (a -> a) -> a -> [a]
iterate = undefined

-- e.g., take 10 $ iterate (*2) 1
\end{code}


`until` takes a predicate and a function, and returns the first value that
satisfies the predicate when repeatedly applied to the function.

\begin{code}
until :: (a -> Bool) -> (a -> a) -> a -> a
until = undefined

-- e.g., until (> 100) (*2) 1
-- e.g., let n = 2 in until (\x -> abs (n-x*x) < 0.001) (\x -> (x + n/x) / 2) 1
\end{code}
