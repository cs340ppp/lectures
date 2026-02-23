module L07HOFs where
import Prelude hiding (($), (.), const, flip, on, even, until,
                       foldr, foldl, foldl')
import Data.Bits (xor)
import Data.Char (ord)
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

until :: (a -> Bool) -> (a -> a) -> a -> a
until p f = go
  where go x | p x       = x
             | otherwise = go (f x)
              
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ z [] = z
foldr f z (x:xs) = f x (foldr f z xs)

sum' :: Num a => [a] -> a
sum' = foldr (+) 0

and' :: [Bool] -> Bool
and' = foldr (&&) True

(+++) :: [a] -> [a] -> [a]
l1 +++ l2 = foldr (:) l2 l1

length' :: [a] -> Int
length' = foldr (\_ n -> n+1) 0

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x xs -> f x : xs) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x xs -> if p x then x:xs else xs) []

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ acc [] = acc
foldl f acc (x:xs) = foldl f (f acc x) xs

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ acc [] = acc
foldl' f acc (x:xs) = let acc' = f acc x
                      in acc' `seq` foldl' f acc' xs

reverse' :: [a] -> [a]
reverse' = foldl' (flip (:)) []

sequenceOps :: Num a => a -> [a -> a] -> a
sequenceOps init = foldl' (\x op -> op x) init

hash :: String -> Integer
hash = foldl' h 0
  where h v c = (7*v `xor` fromIntegral (ord c)) `mod` 1000007
