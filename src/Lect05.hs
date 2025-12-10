module Lect05 where

c2k :: (Ord a, Floating a) => a -> a
c2k c | c >= 0 = c + 273.15
      | otherwise = error "Temperature below absolute zero"


c2f :: Floating a => a -> a
c2f c = c * 9/5 + 32


f2c :: Floating a => a -> a
f2c f = (f - 32) * 5/9


quadRoots :: (Floating a, Ord a) => a -> a -> a -> (a, a)
quadRoots a b c 
    | disc >= 0 = ((-b + sqrt_d) / (2*a), (-b - sqrt_d) / (2*a))
    | otherwise = error "No real roots"
  where disc   = b^2 - 4*a*c
        sqrt_d = sqrt disc


mySum :: (Eq a, Num a) => [a] -> a
mySum [] = 0
mySum (x:xs) = x + mySum xs
