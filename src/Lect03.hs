module Lect03 where
import Data.Char

nand :: Bool -> Bool -> Bool
nand x y = not (x && y)

distance :: (Floating a) => (a, a) -> (a, a) -> a
distance p1 p2 = sqrt ((fst p1 - fst p2)^2 + (snd p1 - snd p2)^2)

not' :: Bool -> Bool
not' True = False
not' False = True


fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)


nand' :: Bool -> Bool -> Bool
nand' False False = True
nand' _ _ = False


fst' :: (a,b) -> a
fst' (x,_) = x

snd' :: (a,b) -> b
snd' (_,y) = y


distance' :: (Floating a) => (a, a) -> (a, a) -> a
distance' (x1,y1) (x2,y2) = sqrt ((x1-x2)^2 + (y1-y2)^2)


mapTup :: (a -> b) -> (a, a) -> (b, b)
mapTup f (x,y) = (f x, f y)


foo :: (a, (b, c)) -> ((a, (b, c)), (b, c), (a, b, c))
foo p@(x, q@(y, z)) = (p, q, (x, y, z))


fib' :: Integer -> Integer
fib' n | n == 0 = 0
       | n == 1 = 1
       | otherwise = fib' (n-1) + fib' (n-2)

letterGrade :: (Ord a, Num a) => a -> Char
letterGrade n | n >= 90   = 'A'
              | n >= 80   = 'B'
              | n >= 70   = 'C'
              | n >= 60   = 'D'
              | otherwise = 'E'
c2f :: (Floating a) => a -> a
c2f c = c * 9 / 5 + 32

c2h :: (Floating a, Ord a) => a -> String
c2h c | f >= 100 = "hot"
      | f >= 70  = "comfortable"
      | f >= 50  = "cool"
      | otherwise    = "cold"
      where f = c2f c

closer :: (Floating a, Ord a) => (a, a) -> (a, a) -> (a, a) -> (a, a)
closer src dst1 dst2 = if d1 < d2 then dst1 else dst2
  where d1 = distance src dst1
        d2 = distance src dst2


quadrant :: (Int, Int) -> Int
quadrant (x, y) | x > 0 && y > 0 = 1
                | x < 0 && y > 0 = 2
                | x < 0 && y < 0 = 3
                | x > 0 && y < 0 = 4
                | otherwise      = 0

quadrantNames :: (Int, Int) -> String
quadrantNames (x, y) = case quadrant (x, y) of 1 -> "All"
                                               2 -> "Science"
                                               3 -> "Teachers"
                                               4 -> "Crazy"
                                               _ -> "Origin"

verboseCompare :: Int -> Int -> String
verboseCompare x y = 
  "Comparing " ++ show x ++ " and " ++ show y ++ ": " ++
  case (abs x, abs y) of
    (0, 0) -> "both zero"
    (0, _) -> "first is zero"
    (_, 0) -> "second is zero"
    (a, b) | a > b     -> "first has larger magnitude"
           | a < b     -> "second has larger magnitude"
           | otherwise -> "equal magnitudes"

quadRoots :: Double -> Double -> Double -> (Double, Double)
quadRoots a b c = let disc = a^2 - 4*a*c
                      x1 = (-b + sqrt disc) / (2*a)
                      x2 = (-b - sqrt disc) / (2*a)
                  in (x1, x2)
