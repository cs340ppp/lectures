module Lect08 where
import Prelude hiding (Word, Maybe, Just, Nothing, Either, Left, Right)
import Data.Char

-- Type synonyms: English terms 

type Letter = Char
type Word = [Letter]
type Sentence = [Word]

phrases :: [Word] -> [Word] -> [Sentence]
phrases adjs nouns = [ [adj, noun]
                       | adj <- adjs, noun <- nouns ]

-- Type synonyms: tuple aliases

type Point2D = (Double, Double)

distance :: Point2D -> Point2D -> Double
distance (x1,y1) (x2,y2) = sqrt $ (x1-x2)^2 + (y1-y2)^2


type Numbered a = (Int, a)

enumerate :: [a] -> [Numbered a]
enumerate xs = zip [1..] xs

-- Algebraic data types

data YesOrNo = Yes | No deriving Show  -- "deriving Show" automates String conversion

not' :: YesOrNo -> YesOrNo
not' Yes = No
not' No  = Yes

(|||) :: YesOrNo -> YesOrNo -> YesOrNo
No ||| No = No
_  ||| _  = Yes

data Box = Box Int Bool String deriving Show

b1 = Box 5 True "hello"
b2 = Box 100 False "goodbye"

boxAdd :: Box -> Box -> Box
boxAdd (Box i1 b1 s1) (Box i2 b2 s2)
       = Box (i1 + i2) (b1 || b2) (s1 ++ s2)

data Shape = Circle Double 
             | Triangle Double Double 
             | Rectangle Double Double deriving Show

area :: Shape -> Double
area (Circle r)      = pi * r^2
area (Triangle h b)  = (h*b)/2
area (Rectangle l w) = l*w

-- Counting values (sums & products)

data T1 = T1V1 | T1V2 | T1V3      deriving Show
data T2 = T2V1 Bool | T2V2 T1     deriving Show
data T3 = T3V Bool T1             deriving Show
data T4 = T4V1 T1 T2 | T4V2 T2 T3 deriving Show

-- Record notation

data Student = Student { firstName :: String
                       , lastName  :: String
                       , studentId :: Integer
                       , grades    :: [Char]
                       } deriving Show

s1 = Student "John" "Doe" 1234 ['A', 'B']

s2 = Student { lastName = "Doe"
             , firstName = "Jane"
             , grades = ['A', 'C']
             , studentId = 2345 }

s3 = s1 { grades = ['B', 'A', 'D'] }

-- Self-referential types

data RussianDoll = Doll String RussianDoll | EmptyDoll deriving Show

d1 = EmptyDoll
d2 = Doll "privyet" EmptyDoll
d3 = Doll "matry" (Doll "osh" (Doll "ka" EmptyDoll))
d4 = Doll "infinity, and beyond!" d4

innerMostMessage :: RussianDoll -> String
innerMostMessage EmptyDoll          = error "No message"
innerMostMessage (Doll m EmptyDoll) = m
innerMostMessage (Doll _ d)         = innerMostMessage d

-- Exercise: implement the following function
--     e.g., insideOut d3 => Doll "ka" (Doll "osh" (Doll "matry" EmptyDoll))
insideOut :: RussianDoll -> RussianDoll
insideOut = undefined

-- Parametric types

data UniversalBox a = UBox a deriving Show

ub1 :: UniversalBox Bool
ub1 = UBox True

ub2 :: UniversalBox [Int]
ub2 = UBox [1..10]

ub3 :: UniversalBox (Int -> Bool)
ub3 = UBox even

mapBox :: (a -> b) -> UniversalBox a -> UniversalBox b
mapBox f (UBox x) = UBox $ f x

-- Exercise: implement the following function
--     e.g., sumBoxes [UBox 2, UBox 4, UBox 6] => UBox 12
sumBoxes :: Num a => [UniversalBox a] -> UniversalBox a
sumBoxes = undefined

-- "Kinds" of types
             
data Bar a b = Bar1 a | Bar2 b
data Bat a b c = Bat a b c
data Baz a b = Baz (a b)

-- Some useful parametric types

-- Maybe

data Maybe a = Just a | Nothing deriving Show

find :: (a -> Bool) -> [a] -> Maybe a
find _ [] = Nothing
find p (x:xs) | p x = Just x
              | otherwise = find p xs

-- Exercise: alter `quadRoots` to return a `Maybe`
quadRoots :: Double -> Double -> Double -> (Double,Double)
quadRoots a b c = let d = b^2-4*a*c
                      sd = sqrt d
                  in if d < 0
                     then error "No real roots"
                     else ((-b+sd)/(2*a), (-b-sd)/(2*a))

-- Either

data Either a b = Left a | Right b deriving Show

find' :: (a -> Bool) -> [a] -> Either String a
find' _ [] = Left "List was empty"
find' p (x:xs) | p x = Right x
               | null xs = Left "No matching element"
               | otherwise = find' p xs

-- Our own list type

infixr 5 :-
data List a = a :- (List a) | Null deriving Show

l1 :: List Char
l1 = 'h' :- 'e' :- 'l' :- 'l' :- 'o' :- Null

l2 :: List Char
l2 = 'h' :- 'a' :- Null

l3 :: List (List Int)
l3 = (1 :- 2 :- Null) :- (3 :- 4 :- Null) :- Null

mapL :: (a -> b) -> List a -> List b
mapL _ Null = Null
mapL f (x :- xs) = f x :- mapL f xs

-- Exercises: implement the following `List` functions:
takeL :: Int -> List a -> List a
takeL = undefined

enumFromL :: (Eq a, Enum a) => a -> List a
enumFromL = undefined
