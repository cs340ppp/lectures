module L07HOFs where
import Prelude hiding (($), (.), const, flip, on, even,
                       iterate, until,
                       foldr, foldl, foldr1, foldl1)
import Data.Function.Memoize

-- Combinators

infixr 0 $
($) :: (a -> b) -> a -> b
f $ x = f x

infixr 9 .
(.) :: (b -> c) -> (a -> b) -> a -> c
(f . g) x = f (g x)

even :: Integral a => a -> Bool
even = (== 0) . (`mod` 2)

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
k2h = f2h . c2f . k2c

const :: a -> b -> a
const x = \_ -> x

flip :: (a -> b -> c) -> b -> a -> c
flip f = \x y -> f y x

on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
on g f = \x y -> g (f x) (f y)

fix :: (a -> a) -> a
fix f = let x = f x in x

ones :: [Int]
ones = fix (1:)

factorial :: Integer -> Integer
factorial = fix (\rec n -> if n == 0 then 1 else n * rec (n-1))

add :: Int -> Int -> Int
add = fix (\rec m n -> if m == 0 then n else rec (m-1) (n+1))

fib :: Integer -> Integer
fib = memoFix (\rec n -> if n == 0 || n == 1 then n
                         else rec (n-1) + rec (n-2))
