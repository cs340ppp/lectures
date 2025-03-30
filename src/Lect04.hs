module Lect04 where
import Data.Char

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate' (n-1) x

enumFromTo' :: (Ord a, Enum a) => a -> a -> [a]
enumFromTo' x y | x <= y    = x : enumFromTo' (succ x) y
                | otherwise = []

ones :: [Int]
ones = 1 : ones
 
repeat' :: a -> [a]
repeat' x = x : repeat' x

enumFrom' :: Enum a => a -> [a]
enumFrom' x = x : enumFrom' (succ x)

evens = [2*x | x <- [1..]]

evens' = [x | x <- [1..], x `mod` 2 == 0]

sudokuBoxes = [[[r,c] | r <- rs, c <- cs] | rs <- ["ABC", "DEF", "GHI"],
                                            cs <- ["123", "456", "789"]]

integerRightTriangles p = [(a,b,c) | a <- [1..p], 
                                     b <- [a..(p-a)],
                                     let c = p-(a+b),
                                     a^2 + b^2 == c^2]

factors :: Integral a => a -> [a]
factors n = [f | f <- [1..n], n `mod` f == 0]

cartesianProduct :: [a] -> [b] -> [(a,b)]
cartesianProduct xs ys = [(x,y) | x <- xs, y <- ys]

concat' :: [[a]] -> [a]
concat' ls = [x | l <- ls, x <- l]

head' :: [a] -> a
head' (x:_) = x

tail' :: [a] -> [a]
tail' (_:xs) = xs

null' :: [a] -> Bool
null' [] = True
null' _  = False

length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

last' :: [a] -> a
last' (x:[]) = x
last' (_:xs) = last' xs


(+++) :: [a] -> [a] -> [a]
[] +++ ys = ys
(x:xs) +++ ys = x : xs +++ ys


(!!!) :: [a] -> Int -> a 
(x:_) !!! 0 = x
(_:xs) !!! n = xs !!! (n-1)


reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs +++ [x] -- is there a more efficient way?


take' :: Int -> [a] -> [a]
take' 0 _ = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs


splitAt' :: Int -> [a] -> ([a], [a])
splitAt' _ [] = ([],[])
splitAt' 0 xs = ([], xs)
splitAt' n (x:xs) = let (ys,zs) = splitAt' (n-1) xs
                    in (x:ys, zs)


break' :: (a -> Bool) -> [a] -> ([a], [a])
break' _ [] = ([],[])
break' p l@(x:xs) | p x = ([], l)
                  | otherwise = let (ys, zs) = break' p xs
                                in (x:ys, zs)

words' :: String -> [String]
words' [] = []
words' l@(c:cs) | isSpace c = words' cs
                | otherwise = let (w, ws) = break' isSpace l
                              in w : words' ws

caesar :: Int -> String -> String
caesar _ [] = []
caesar n (x:xs) = (if isLetter x then encrypt x else x) : caesar n xs
  where encrypt x = n2l ((l2n x + n) `mod` 26)
        l2n c = ord (toUpper c) - ord 'A'
        n2l n = chr (n + ord 'A')
