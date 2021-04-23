-- CS 340: Programming Paradigms and Patterns
-- Lect 11 - Monadic Parsing
-- Michael Lee

module Lect.Lect11Complete where
import Data.Char

data State s a = State { run :: s -> Maybe (a, s) }


instance Functor (State s) where
  fmap f st = State $ \s -> case run st s of
                              Nothing -> Nothing
                              Just (x, s') -> Just (f x, s')


instance Applicative (State s) where
  pure x = State $ \s -> Just (x, s)
  stf <*> stx = State $ \s -> case run stf s of
                                Nothing -> Nothing
                                Just (f, s') -> run (f <$> stx) s'


instance Monad (State s) where
  st >>= f = State $ \s -> case run st s of
                             Nothing -> Nothing
                             Just (x, s') -> run (f x) s'


type Parser a = State String a


item :: Parser Char
item = State $ \input -> case input of "" -> Nothing
                                       (x:xs) -> Just (x, xs)


sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else State (\_ -> Nothing)


char :: Char -> Parser Char
char c = sat (==c)


string :: String -> Parser String
string "" = return ""
string (x:xs) = do char x
                   string xs
                   return (x:xs)


digit :: Parser Char
digit = sat isDigit


digits :: Parser [Char]
digits = do d <- digit
            ds <- digits
            return $ d:ds


pOr :: Parser a -> Parser a -> Parser a
p `pOr` q = State $ \s -> case run p s of 
                               Nothing -> run q s
                               r -> r


digits' :: Parser [Char]
digits' = do d <- digit 
             ds <- digits' `pOr` return []
             return $ d:ds


onePlus :: Parser a -> Parser [a]
onePlus p = do x <- p 
               xs <- onePlus p `pOr` return []
               return $ x:xs


digits'' = onePlus digit


zeroPlus :: Parser a -> Parser [a]
zeroPlus p = onePlus p `pOr` return []


onePlus' :: Parser a -> Parser [a]
onePlus' p = pure (:) <*> p <*> zeroPlus p


digits''' = onePlus' digit


class Applicative f => Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a

  many :: f a -> f [a]
  some :: f a -> f [a]

  many x = some x <|> pure []
  some x = pure (:) <*> x <*> many x


instance Alternative (State s) where
  empty = State $ \s -> Nothing
  p <|> q = State $ \s -> case run p s of
                            Nothing -> run q s
                            r -> r

sat' p = do x <- item
            if p x then return x else empty

digits'''' :: Parser [Char]
digits'''' = some digit


nat :: Parser Int
nat = read <$> digits''''


int :: Parser Int
int = (do char '-'
          n <- nat
          return (-n))
     <|> nat


space :: Parser ()
space = do many (sat isSpace)
           return ()


token :: Parser a -> Parser a
token p = do space
             x <- p
             space
             return x


symbol :: String -> Parser String
symbol s = token (string s)


integer :: Parser Int
integer = token int


ints :: Parser [Int]
ints = do symbol "["
          n <- integer
          ns <- many (do symbol ","
                         integer)
          symbol "]"
          return (n:ns)
