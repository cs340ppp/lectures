{-# LANGUAGE FlexibleInstances, InstanceSigs #-}

module L09Classes where
import Data.Char

-- Our first type class

class Explosive a where
  explode :: a -> [a]

  explodeTwice :: a -> [[a]]
  explodeTwice = map explode . explode 

-- Instances of our first type class

instance Explosive Integer where
  explode :: Integer -> [Integer]
  explode n = [1..n]

instance Explosive [a] where
  explode :: [a] -> [[a]]
  explode = map (:[])

-- Using our type class as a constraint

mapExploded :: Explosive a => (a -> b) -> a -> [b]
mapExploded f x = map f $ explode x

-- Some data types from the previous lecture 

data Student = Student { firstName :: String
                       , lastName  :: String
                       , studentId :: Integer
                       , grades    :: [Char]
                       } deriving Show

s1 = Student "John" "Doe"  12345678 ['B', 'C', 'C']
s2 = Student "Mary" "Jane" 23456789 ['A', 'B', 'A']

infixr 5 :-
data List a = a :- (List a) | Null deriving Show

l1 = 1 :- 2 :- 3 :- 4 :- 5 :- Null
l2 = "hello" :- "how" :- "you" :- "doing" :- Null

-- Making Student an instance of Eq and Ord

instance Eq Student where
  (==) :: Student -> Student -> Bool
  (Student _ _ id1 _) == (Student _ _ id2 _) = id1 == id2

instance Ord Student where
  compare :: Student -> Student -> Ordering
  compare (Student _ _ id1 _) (Student _ _ id2 _) = compare id1 id2

-- Making List an instance of Eq and Foldable

instance Eq a => Eq (List a) where
  (==) :: Eq a => List a -> List a -> Bool
  Null == Null = True
  (x :- xs) == (y :- ys) = x == y && xs == ys
  _ == _ = False

instance Foldable List where
  foldr :: (a -> b -> b) -> b -> List a -> b
  foldr _ v Null = v
  foldr f v (x :- xs) = f x $ foldr f v xs

-- Automatically generate instance implementations with `deriving`

data Suit = Diamond | Club | Heart | Spade 
            deriving (Eq, Ord, Enum, Bounded, Show, Read)
