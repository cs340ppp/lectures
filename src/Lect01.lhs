% CS 340: Programming Paradigms and Patterns
% Lect 01 - Introduction to Haskell
% Michael Lee

> module Lect01 where
> import Data.List

Introduction to Haskell
=======================

Agenda:
  - working with literate source files
  - general development workflow and tools
  - notable (& maybe surprising) language features
  - indentation and layout rules


Literate Haskell
----------------

Regular Haskell source code filenames end in ".hs" --- all your assignments
will use this extension --- but they can also end in ".lhs", which denotes a
"literate" Haskell source file. In a literate source file, all text defaults
to being a comment; lines of source code must start with the ">" character,
and must also be separated from other text by at least one empty line.

All lecture notes will be provided as literate source files. Here's what code
would look like in a literate source file:

> welcome = "Welcome to CS 340!"

You can also surround code with begin/end markers, like this:

\begin{code}
  courseName = "Programming Paradigms and Patterns"
\end{code}


Development workflow and tools
------------------------------

You'll need to switch frequently between multiple tools and platforms to
do your work in this class. They include:

- Git, a version control system

- GitHub, a Git hosting service

- GHC, the Glasgow Haskell Compiler

- Stack, a Haskell build tool

- a source code editor with Haskell support --- I strongly recommend Visual Studio Code with the Haskell plugin


Notable (& maybe surprising) language features
----------------------------------------------

1. Strong static typing: Every expression or variable has a type associated with
                         it that doesn't change, and is rigidly enforced by the
                         compiler. Haskel programs are type-safe; i.e., there
                         will never be run-time type related errors!


2. Type inference: The compiler can figure out the types of many things, so that
                   we don't have to expliclty label them.

> -- use the :t GHCi command to get the type of each of the below
>
> mysteryVar1 = 123 `mod` 8
>
> mysteryVar2 = words "hello how are you?"
> 
> mysteryFn1 = (^2)
>
> mysteryFn2 = length . words
> 
> mysteryFn3 f s = [(abs $ f $ length w, w) | w <- words s]


3. Purely functional: Once variables are bound to values they cannot be changed
                      (i.e., variables are immutable).

> boundVar = 10
> -- boundVar = 20 -- error!
 

4. Lazy evaluation: Expressions (e.g., function calls) are not evaluated until
                    their results are strictly needed. Unevaluated computations,
                    called "thunks", are maintained in a graph.

> possiblyTragic c = let e = error "Eeek!"
>                        u = undefined
>                    in case c of 'e' -> e
>                                 'u' -> u
>                                 otherwise -> "safe"
>
> safeDiv x y = let q = x `div` y
>               in if y == 0
>                  then 0
>                  else q


5 . Order-independence: The order bindings appear in code doesn't matter.

> w = x + 2
> x = w - 5
>
> evenN 0 = True
> evenN n = oddN (n-1)
>
> oddN 0 = False
> oddN n = evenN (n-1)


6. Concise

   - Small language with few keywords:

       case  class  data   deriving  do   else  if
       import  in  infix  infixl  infixr  instance
       let  module  newtype  of  then  type  where

   - Declarative vs. Imperative style!

> palindromes = sortOn snd
>               . map (\w -> (w,length w))
>               . filter (\s -> reverse s == s)
>               . words 


Indentation & Layout rules
--------------------------

Haskell supports the use of semicolons and curly braces for delineating
and separating blocks of code, but they are rarely used. Instead, we prefer 
to take advantage of *layout rules* that use indentation to group and separate 
code. 

The "golden rule" for Haskell indentation is:

   Code which is part of some expression should be indented further in 
   than the beginning of that expression.

In addition, all expressions that belong to the same group must be left-aligned
to the same column. The "do", "let", "where", and "of" keywords indicate the
start of group.

> doGuessing num = do
>   putStrLn "Enter your guess:"
>   guess <- getLine
>   case compare (read guess) num of
>     LT -> do putStrLn "Too low!"
>              doGuessing num
>     GT -> do putStrLn "Too high!"
>              doGuessing num
>     EQ -> putStrLn "You Win!"

Read the [Haskell Wikibook](https://en.wikibooks.org/wiki/Haskell/Indentation)
for a briefer on the layout rule, and the 
[Haskell language report](https://www.haskell.org/onlinereport/haskell2010/haskellch2.html#x7-210002.7)
for all the gory details.
