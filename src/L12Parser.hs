module L12Parser where

import Data.Char

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \s ->
    case p s of
      Nothing -> Nothing
      Just (x, s') -> Just (f x, s')

instance Applicative Parser where
  pure x = Parser $ \s -> Just (x, s) 

  Parser pf <*> Parser px = Parser $ \s ->
    case pf s of
      Nothing -> Nothing
      Just (f, s') ->
        case px s' of
          Nothing -> Nothing
          Just (x, s'') -> Just (f x, s'')

instance Monad Parser where
  return = pure

  Parser p >>= f = Parser $ \s ->
    case p s of
      Nothing -> Nothing
      Just (x, s') -> runParser (f x) s'

-- fundamental/primitive parsers
        
item :: Parser Char 
item = Parser $ \input -> 
  case input of
    "" -> Nothing
    (c:cs) -> Just (c, cs)

failure :: Parser a
failure = Parser $ \_ -> Nothing

-- parser utilities

satisfy :: (Char -> Bool) -> Parser Char
satisfy pred = do
  c <- item
  if pred c then return c else failure 
-- satisfy pred = item >>= \c -> (if pred c then return c else failure) 

char :: Char -> Parser Char
char c = satisfy (== c)

digit :: Parser Char
digit = satisfy isDigit

letter :: Parser Char
letter = satisfy isAlpha

string :: String -> Parser String
string "" = return ""
string (c:cs) = do
  char c
  string cs
  return (c:cs)

-- alternative parsing utility

infixl 3 <|>
(<|>) :: Parser a -> Parser a -> Parser a
Parser p <|> Parser q = Parser $ \input -> 
  case p input of
    Nothing -> q input -- p fails, try q
    result -> result -- p succeeds

-- parse zero or more occurrences
many :: Parser a -> Parser [a]
many p = some p <|> return []

-- parser one or more occurrences
some :: Parser a -> Parser [a]
some p = do
  x <- p
  xs <- many p
  return (x : xs)

-- allows us to ignore spaces
spaces :: Parser ()
spaces = do many (satisfy isSpace)
            return ()

token :: Parser a -> Parser a
token p = spaces >> p

symbol :: String -> Parser String
symbol s = token (string s)

-- number parsers

natural :: Parser Int
natural = token $
          do ds <- some digit
             return (read ds)

-- integers allow for an optional leading '-'
integer :: Parser Int
integer = token $
          do sign <- (char '-' <|> return '+') 
             n <- natural
             return $ if sign == '+' then n else (negate n)

-- parses expressions of form "123 + 42"
expr :: Parser Int
expr = do
  x <- integer
  (do op <- operation
      y <- expr
      return $ op x y)
    <|> return x

operation :: Parser (Int -> Int -> Int)
operation = do symbol "+"
               return (+) 
          <|> do symbol "*"
                 return (*)
          <|> do symbol "/"
                 return div
          <|> do symbol "-"
                 return (-)
