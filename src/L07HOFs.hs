module L07HOFs where
import Prelude hiding (($), (.), flip, on, and,
                       map, filter, any, all, iterate, until,
                       foldr, foldl, foldr1, foldl1)
import Data.Char
import Data.Bits ( Bits(xor) )
import Data.Function hiding (($), (.), flip, on)
import Data.List (minimumBy)
import Debug.Trace

($) :: (a -> b) -> a -> b
infixr 0 $
($) = undefined

(.) :: (b -> c) -> (a -> b) -> a -> c
infixr 9 .
(.) = undefined

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

flip :: (a -> b -> c) -> b -> a -> c
flip = undefined


on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
on = undefined

map :: (a -> b) -> [a] -> [b]
map = undefined

filter :: (a -> Bool) -> [a] -> [a]
filter = undefined

all :: (a -> Bool) -> [a] -> Bool
all = undefined

any :: (a -> Bool) -> [a] -> Bool
any = undefined

sort :: Ord a => [a] -> [a]
sort [] = []
sort (x:xs) = sort [y | y <- xs, y < x] 
              ++ [x] 
              ++ sort [y | y <- xs, y >= x]


sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy = undefined

and :: [Bool] -> Bool
and [] = True
and (x:xs) = (&&) x $ and xs


showCat :: Show a => [a] -> String
showCat [] = ""
showCat (x:xs) = ((++) . show) x $ showCat xs

foldr = undefined

and' :: [Bool] -> Bool
and' = undefined


showCat' :: Show a => [a] -> String
showCat' = undefined


(+++) :: [a] -> [a] -> [a]
l1 +++ l2 = undefined


length' :: [a] -> Int
length' = undefined


map' :: (a -> b) -> [a] -> [b]
map' f = undefined


filter' :: (a -> Bool) -> [a] -> [a]
filter' p = undefined

foldrT :: (Show a, Show b) => (a -> b -> b) -> b -> [a] -> b
foldrT _ v [] = v
foldrT f v (x:xs) = let e  = trace ("<" ++ show x ++ ">") x
                    in trace "R" $ f e $ foldrT f v xs

hash :: Integer -> String -> Integer
hash seed [] = seed
hash seed (c:cs) = hash (h seed c) cs
  where h v c = (7*v `xor` fromIntegral (ord c)) `mod` 1000007


playMoves :: [Char] -> [(Int,Char)] -> [Char]
playMoves board [] = board
playMoves board (m:moves) = playMoves (move board m) moves
  where move board (x,y) = take x board ++ [y] ++ drop (x+1) board

foldl = undefined

hash' :: String -> Integer
hash' = undefined


playMoves' :: [(Int,Char)] -> [Char]
playMoves' = undefined

foldlT :: (Show a, Show b) => (b -> a -> b) -> b -> [a] -> b
foldlT _ v [] = v
foldlT f v (x:xs) = let e  = trace ("<" ++ show x ++ ">") x
                        a  = f v e
                        a' = trace ("<<" ++ show a ++ ">>") a
                    in trace "R" $ foldlT f a' xs
foldlTS :: (Show a, Show b) => (b -> a -> b) -> b -> [a] -> b
foldlTS = undefined

foldr1 :: (a -> a -> a) -> [a] -> a
foldr1 = undefined

-- e.g., foldr1 (*) [1..5]
--       foldr1 (^) [2,2,3]


foldl1 :: (a -> a -> a) -> [a] -> a
foldl1 = undefined

-- e.g., foldl1 (++) [[1,2], [3,4], [5,6]]
--       foldl1 (/) [16,2,4]
iterate :: (a -> a) -> a -> [a]
iterate = undefined

-- e.g., take 10 $ iterate (*2) 1
until :: (a -> Bool) -> (a -> a) -> a -> a
until = undefined

-- e.g., until (> 100) (*2) 1
-- e.g., let n = 2 in until (\x -> abs (n-x*x) < 0.001) (\x -> (x + n/x) / 2) 1
