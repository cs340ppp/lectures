module L03Functions where

-- lambda notation for anonymous functions

inc = \x -> x + 1

incForever = \x -> incForever x + 1

-- syntactic sugar: functions as equations

nand :: Bool -> Bool -> Bool
nand x y = not (x && y)

c2f :: (Floating a) => a -> a
c2f c = c * 9 / 5 + 32

-- operators are functions

(<<) :: (Ord a, Num a) => a -> a -> Bool
x << y = x*10 < y

infixl 6 +++
(+++) :: String -> String -> String
s1 +++ s2 = s1 ++ ":::" ++ s2

infixl 7 ***
(***) :: Char -> Int -> String
c *** n = replicate n c

-- pattern matching

not' :: Bool -> Bool
not' True  = False
not' False = True

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

nand' :: Bool -> Bool -> Bool
nand' False False = True
nand' _ _ = False

-- destructuring

fst' :: (a, b) -> a
fst' (x, _) = x

snd' :: (a, b) -> b
snd' (_, y) = y

distance :: (Floating a) => (a, a) -> (a, a) -> a
distance p1 p2 = sqrt ((fst p1 - fst p2)^2 + (snd p1 - snd p2)^2)

distance' :: (Floating a) => (a, a) -> (a, a) -> a
distance' (x1, y1) (x2, y2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)

-- partial application

distanceFromOrigin :: (Floating a) => (a, a) -> a
distanceFromOrigin = distance (0,0)

-- catch-all case

classifyPoint :: (Int, Int) -> String
classifyPoint (0, 0) = "origin"
classifyPoint (_, 0) = "x-axis"
classifyPoint (0, _) = "y-axis"
classifyPoint _      = "off-axis"

-- partial function raising an error

get3 :: Int -> (a, a, a) -> a
get3 0 (x, _, _) = x
get3 1 (_, y, _) = y
get3 2 (_, _, z) = z
get3 _ _         = error "get3: index out of bounds"

-- as-pattern

pointWithDistance :: (Floating a) => (a, a) -> ((a, a), a)
pointWithDistance p@(x, y) = (p, sqrt (x^2 + y^2))

-- guards

letterGrade :: (Ord a, Num a) => a -> Char
letterGrade n | n >= 90   = 'A'
              | n >= 80   = 'B'
              | n >= 70   = 'C'
              | n >= 60   = 'D'
              | otherwise = 'E'

-- where clause

c2h :: (Floating a, Ord a) => a -> String
c2h c | f >= 100   = "hot"
      | f >= 70    = "comfortable"
      | f >= 50    = "cool"
      | otherwise  = "cold"
  where f = c2f c

-- if-then-else expression

c2fishing :: (Floating a, Ord a) => a -> String
c2fishing c = "Today is a "
         ++ (if f > 60 && f < 90 then "nice" else "bad")
         ++ " day for fishing."
  where f = c2f c

-- case expression

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

-- let-in expression

quadRoots :: Double -> Double -> Double -> (Double, Double)
quadRoots a b c = let discriminant = b^2 - 4*a*c
                      sqrtDisc = sqrt discriminant
                  in ((-b + sqrtDisc) / 2*a, (-b - sqrtDisc) / 2*a)

-- where vs. let-in

bmi weight height
  | bmi < 18.5 = "underweight"
  | bmi < 25.0 = "normal"
  | bmi < 30.0 = "overweight"
  | otherwise  = "obese"
  where bmi = weight / height^2

cylinderVolume r h = 
  let baseArea = pi * r^2
  in baseArea * h
