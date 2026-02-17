module L04Lists where
import Prelude hiding (null, head, tail, length, product, enumFromTo,
                       map, filter, zip, (++), reverse, repeat,
                       enumFrom, take, drop)

-- basic list construction

intList :: [Int]
intList = 1 : 2 : 3 : 4 : 5 : []

str1, str2 :: [Char]
str1 = "world"
str2 = "hello" ++ str1

vowels :: [(Char, Bool)]
vowels = [('a', True), ('b', False), ('e', True)]

counts :: [[Int]]
counts = [[1], [1,2], [1,2,3]]

-- basic pattern matching

null :: [a] -> Bool
null [] = True
null _  = False

head :: [a] -> a
head (x:_) = x
head _ = error "empty list"

tail :: [a] -> [a]
tail (_:xs) = xs
tail _ = error "empty list"

-- deep pattern matching

sumSecondTwo :: Num a => [a] -> a
sumSecondTwo (_:x:y:_) = x + y
sumSecondTwo _ = 0

firstOfFirst :: [[a]] -> a
firstOfFirst ((x:_):_) = x
firstOfFirst _ = error "bad structure"

-- structural recursion

length :: [a] -> Int
length [] = 0
length (x:xs) = 1 + length xs

productOfSquares :: Num a => [a] -> a
productOfSquares [] = 1
productOfSquares (x:xs) = x^2 * productOfSquares xs

-- building lists

enumFromTo :: (Ord a, Enum a) => a -> a -> [a]
enumFromTo x y | x <= y    = x : enumFromTo (succ x) y
               | otherwise = []

-- processing & building lists

double :: Num a => [a] -> [a]
double [] = []
double (x:xs) = 2*x : double xs

-- generalized function mapping HOF

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs

-- filtering HOF

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x:xs)
  | p x       = x : filter p xs
  | otherwise = filter p xs

-- traversing two lists in parallel

zip :: [a] -> [b] -> [(a, b)]
zip [] _ = []
zip _ [] = []
zip (x:xs) (y:ys) = (x,y) : zip xs ys

-- concatenation

(++) :: [a] -> [a] -> [a]
xs ++ [] = xs
[] ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)

-- reversal

-- O(N^2) version (inefficient!)
reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

-- O(N) using an accumulator
reverse':: [a] -> [a] -> [a]
reverse' (x:xs) acc = reverse' xs (x:acc)
reverse' [] acc = acc

-- hiding the accumulator with a helper function
reverse'' :: [a] -> [a]
reverse'' lst = aux lst []
  where aux (x:xs) acc = aux xs (x:acc)
        aux [] acc = acc

-- infinite list generators

repeat :: a -> [a]
repeat x = x : repeat x

enumFrom :: Enum a => a -> [a]
enumFrom x = x : enumFrom (succ x)

-- a useful pair of functions

take :: Int -> [a] -> [a]
take 0 _ = []
take _ [] = []
take n (x:xs) = x : take (n-1) xs

drop :: Int -> [a] -> [a]
drop 0 xs = xs
drop _ [] = []
drop n (x:xs) = drop (n-1) xs

-- list comprehensions

pairs :: [a] -> [b] -> [(a, b)]
pairs xs ys = [(x, y) | x <- xs, y <- ys]

orderedPairs :: [Int] -> [(Int, Int)]
orderedPairs xs = [(x, y) | x <- xs, y <- xs, x < y]

-- more infinite lists

oddSquares :: [Integer]
oddSquares = map (^2) (filter odd [1..])

primes :: [Integer]
primes = sieve [2..]
  where
    sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p > 0]
    
fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

