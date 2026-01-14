# Domain-Specific Languages

## Agenda

**Lecture 1: DSL Concepts & Design**

- What is a DSL?
- Internal vs. External DSLs
- When to Build a DSL
- Design Principles

**Lecture 2: Shallow Embedding**

- What is Shallow Embedding?
- Building DSLs with Combinators
- Type-Safe DSL Design
- Case Study: Query DSL

**Lecture 3: Deep Embedding & Interpreters**

- What is Deep Embedding?
- Building an AST
- Writing Interpreters
- Multiple Interpretations
- Optimization and Transformation

## LECTURE 1: DSL CONCEPTS & DESIGN

## What is a Domain-Specific Language?

A *Domain-Specific Language (DSL)* is a programming language specialized for a particular domain or problem.

**Contrast with General-Purpose Languages (GPLs)**:

- GPLs: Java, Python, Haskell—designed for any problem
- DSLs: SQL, HTML, CSS, regex—designed for specific problems

DSLs trade generality for expressiveness in their domain.

## Why Build a DSL?

1. **Domain experts can express solutions naturally**: Match their mental model

2. **Conciseness**: Say more with less code

3. **Safety**: Prevent invalid constructs at compile time

4. **Optimization**: Domain knowledge enables specialized optimizations

5. **Abstraction**: Hide implementation details

## DSL Examples in the Wild

**SQL**: Database queries

```sql
SELECT name, age FROM users WHERE age > 18 ORDER BY name;
```

**HTML**: Document structure

```html
<div class="container"><h1>Title</h1><p>Content</p></div>
```

**Regular Expressions**: Pattern matching

```
^\d{3}-\d{2}-\d{4}$
```

**Makefile**: Build automation

```make
target: dependencies
    command
```

## DSLs in Haskell

Haskell is particularly well-suited for building DSLs:

**Parser Combinators** (`Parsec`, `Megaparsec`):

```haskell
parseDate = do
  year <- count 4 digit
  char '-'
  month <- count 2 digit
  char '-'
  day <- count 2 digit
  return (year, month, day)
```

Looks like a parser specification, but it's Haskell code!

## More Haskell DSLs

**QuickCheck**: Property testing

```haskell
prop_reverse :: [Int] -> Bool
prop_reverse xs = reverse (reverse xs) == xs
```

**Diagrams**: Vector graphics

```haskell
example = circle 1 # fc blue ||| square 2 # fc red
```

**Hakyll**: Static site generation

```haskell
match "posts/*" $ do
  route $ setExtension "html"
  compile $ pandocCompiler >>= loadAndApplyTemplate "templates/post.html"
```

## Internal vs. External DSLs

**External DSL**: A separate language with its own syntax

- Examples: SQL, HTML, CSS
- Requires a custom parser
- Complete control over syntax
- More work to implement

**Internal DSL** (Embedded DSL): Implemented within a host language

- Examples: Parsec, QuickCheck, Diagrams
- Uses host language syntax
- Leverages host language features (types, functions, etc.)
- Easier to implement

We'll focus on *internal DSLs* in Haskell.

## Advantages of Internal DSLs

1. **No parser needed**: Use host language syntax

2. **Reuse host features**: Types, functions, modules, etc.

3. **Tool support**: Editor support, debugger, profiler

4. **Interoperability**: Easy to mix with regular code

5. **Rapid development**: Faster to prototype and iterate

## Disadvantages of Internal DSLs

1. **Syntax constraints**: Bound by host language syntax

2. **Error messages**: May reference host language concepts

3. **Learning curve**: Users need some host language knowledge

4. **Less control**: Can't customize syntax freely

Despite these limitations, internal DSLs are often the right choice.

## When to Build a DSL

Build a DSL when:

1. **Repeated patterns**: You keep writing similar code

2. **Domain complexity**: The domain has intricate rules

3. **Non-programmers**: Domain experts aren't coders

4. **Configuration**: Complex configuration needs structure

5. **Safety**: Domain constraints should be enforced

Don't build a DSL when:

- A library would suffice
- The domain is too simple
- Maintenance burden exceeds benefits

## DSL Design Principles

1. **Domain-driven**: Match the domain's concepts and vocabulary

2. **Composability**: Small pieces combine to form larger expressions

3. **Type safety**: Leverage types to prevent invalid constructs

4. **Abstraction layers**: Hide complexity behind clean interfaces

5. **Progressive disclosure**: Simple things simple, complex things possible

## Example: A Simple Arithmetic DSL

Let's design a DSL for arithmetic expressions.

**What we want to express**:

```haskell
(2 + 3) * 4
5 - (1 + 2)
```

**Requirements**:

- Support addition, subtraction, multiplication
- Evaluate expressions
- Pretty-print expressions
- Eventually: optimize, compile, etc.

## Two Approaches to DSL Implementation

**Shallow Embedding**:

- Represent programs by their *denotation* (meaning)
- Operations are functions that compute results directly
- Example: An expression is a function `() -> Int`

**Deep Embedding**:

- Represent programs as *abstract syntax trees* (ASTs)
- Operations build tree nodes
- Interpretation is a separate step
- Example: An expression is a data structure we can inspect

We'll explore both approaches in depth!

## Shallow vs. Deep: A Preview

**Shallow Embedding**:

```haskell
type Expr = Int
add x y = x + y
eval e = e
```

**Deep Embedding**:

```haskell
data Expr = Lit Int | Add Expr Expr
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2
```

Each has different tradeoffs. Let's explore them systematically.

## Case Study: Regular Expressions

Consider a DSL for regular expressions.

**Domain concepts**:

- Character literals: `'a'`, `'b'`
- Alternatives: `a | b`
- Sequencing: `ab`
- Repetition: `a*`, `a+`
- Groups: `(ab)*`

**Domain operations**:

- Match a string
- Find all matches
- Replace matches

## Regular Expression DSL: Shallow Approach

```haskell
type Regex = String -> Bool

-- Primitives
char :: Char -> Regex
char c = \s -> not (null s) && head s == c

-- Combinators
(|||) :: Regex -> Regex -> Regex
(r1 ||| r2) s = r1 s || r2 s

(<*>) :: Regex -> Regex -> Regex
(r1 <*> r2) s = or [r1 (take k s) && r2 (drop k s) | k <- [0..length s]]
```

Each operation directly implements its semantics!

## Regular Expression DSL: Deep Approach

```haskell
data Regex
  = Char Char
  | Alt Regex Regex
  | Seq Regex Regex
  | Star Regex

-- Build expressions
char :: Char -> Regex
char = Char

(|||) :: Regex -> Regex -> Regex
(|||) = Alt

-- Interpret later
eval :: Regex -> String -> Bool
eval (Char c) s = not (null s) && head s == c
eval (Alt r1 r2) s = eval r1 s || eval r2 s
-- ... etc
```

## Tradeoffs: Shallow vs. Deep

**Shallow Embedding**:

- ✅ Simple to implement
- ✅ Natural embedding in host language
- ❌ Hard to inspect or transform programs
- ❌ Only one interpretation

**Deep Embedding**:

- ✅ Can inspect and transform AST
- ✅ Multiple interpretations (eval, optimize, pretty-print, etc.)
- ❌ More boilerplate (data types, constructors)
- ❌ Potential overhead from tree construction

## Choosing an Embedding Strategy

Use **shallow embedding** when:

- You need one interpretation
- You want minimal overhead
- You value simplicity

Use **deep embedding** when:

- You need multiple interpretations
- You need to analyze/optimize programs
- You want to transform expressions
- You need to serialize programs

Sometimes you can start shallow and refactor to deep later!

## Exercise: DSL Identification

For each of the following, identify whether it's better suited for shallow or deep embedding:

1. A DSL for generating random test data
2. A DSL for SQL queries that need optimization
3. A DSL for configuring HTTP routes
4. A DSL for 2D graphics that need to be rendered to multiple formats
5. A DSL for validation rules on forms

// 1. Shallow (just generate directly)
// 2. Deep (need to optimize queries)
// 3. Shallow (just register handlers)
// 4. Deep (need multiple interpretations: SVG, Canvas, etc.)
// 5. Could be either, but deep allows better error reporting

## LECTURE 2: SHALLOW EMBEDDING

## What is Shallow Embedding?

In shallow embedding, a DSL program is represented by its *denotation*—what it means.

**Key idea**: DSL operations are functions that directly compute results.

Example: An HTML DSL

```haskell
type HTML = String

tag :: String -> HTML -> HTML
tag name content = "<" ++ name ++ ">" ++ content ++ "</" ++ name ++ ">"

p :: HTML -> HTML
p = tag "p"

div :: HTML -> HTML
div = tag "div"
```

## Building a Query DSL (Shallow)

Let's build a simple DSL for database queries.

**Domain**: Query a list of records

**Operations**:

- Select specific columns
- Filter rows
- Sort results
- Join tables

## Query DSL: Core Types

```haskell
-- A database table is just a list of records
type Table a = [a]

-- A query produces a table when given a source table
type Query a b = Table a -> Table b
```

A `Query a b` transforms a table of `a` into a table of `b`.

## Query DSL: Primitives

```haskell
-- Select all records (identity query)
selectAll :: Query a a
selectAll = id

-- Transform each record
selectMap :: (a -> b) -> Query a b
selectMap f = map f

-- Filter records
whereFilter :: (a -> Bool) -> Query a a
whereFilter predicate = filter predicate
```

These are just function compositions on lists!

## Query DSL: Composition

```haskell
-- Compose queries sequentially
(<|>) :: Query b c -> Query a b -> Query a c
(<|>) = (.)

-- Example: Select and then filter
query1 :: Query Person PersonName
query1 = selectMap getName <|> whereFilter isActive
```

The `<|>` operator is just function composition (`.`)! This makes queries naturally composable.

## Query DSL: Example Usage

```haskell
data Person = Person
  { name :: String
  , age :: Int
  , active :: Bool
  }

-- Define queries
adults :: Query Person Person
adults = whereFilter (\p -> age p >= 18)

names :: Query Person String
names = selectMap name

-- Compose them
adultNames :: Query Person String
adultNames = names <|> adults

-- Execute query
main = do
  let people = [Person "Alice" 30 True, Person "Bob" 15 True]
  print $ adultNames people
  -- Output: ["Alice"]
```

## Advantages of Shallow Embedding

1. **Simple implementation**: Uses host language functions directly

2. **Efficient**: No overhead from building intermediate structures

3. **Natural composition**: Function composition = query composition

4. **Type safety**: Type system ensures queries are well-formed

## Limitations of Shallow Embedding

However, this shallow embedding has limitations:

```haskell
-- Can we optimize this query?
query = selectMap f <|> whereFilter p <|> selectMap g
```

With shallow embedding, we can't:

- Inspect what the query does
- Optimize (e.g., fusing maps)
- Generate SQL
- Pretty-print for debugging

The query is just an opaque function!

## Combinator Pattern

The *combinator pattern* is central to shallow DSLs:

1. **Primitives**: Basic building blocks
   ```haskell
   char :: Char -> Parser Char
   ```

2. **Combinators**: Functions that combine primitives
   ```haskell
   (<|>) :: Parser a -> Parser a -> Parser a
   many :: Parser a -> Parser [a]
   ```

3. **Derived combinators**: Built from primitives and other combinators
   ```haskell
   word = many (satisfy isAlpha)
   ```

## Combinator Laws

Well-designed combinators should satisfy algebraic laws:

```haskell
-- Associativity
(a <|> b) <|> c  ==  a <|> (b <|> c)

-- Identity
selectAll <|> q  ==  q
q <|> selectAll  ==  q

-- Composition
(f <|> g) <|> h  ==  f <|> (g <|> h)
```

Laws help reason about DSL programs and enable optimizations (if we could inspect them!).

## Type-Safe DSL Design

Haskell's type system can encode domain constraints:

```haskell
-- A query that requires a sorted input
type SortedQuery a b = Sorted (Table a) -> Table b

-- A query that produces sorted output
type SortingQuery a b = Table a -> Sorted (Table b)

-- Sorted is a phantom type
newtype Sorted a = Sorted a
```

This prevents running queries that assume sorted data on unsorted tables!

## Type-Safe Query DSL

```haskell
-- Sort a table
sort :: Ord a => Query a (Sorted a)
sort xs = Sorted (List.sort xs)

-- A query that requires sorted input
median :: Query (Sorted a) a
median (Sorted xs) = xs !! (length xs `div` 2)

-- Composition enforces sorting
validQuery :: Ord a => Query a a
validQuery = median <|> sort

-- This won't compile!
-- invalidQuery = median <|> selectAll  -- Type error!
```

## Case Study: HTML DSL (Shallow)

```haskell
type HTML = String

-- Primitives
text :: String -> HTML
text = id

tag :: String -> HTML -> HTML
tag name content = "<" ++ name ++ ">" ++ content ++ "</" ++ name ++ ">"

-- Combinators
(<>) :: HTML -> HTML -> HTML
(<>) = (++)

-- Derived combinators
p, div, span :: HTML -> HTML
p = tag "p"
div = tag "div"
span = tag "span"
```

## HTML DSL: Example

```haskell
page :: HTML
page =
  div $
    p (text "Hello, ") <> span (text "world") <>
    p (text "Welcome to my page!")

-- Render
main = putStrLn page
-- Output: <div><p>Hello, </p><span>world</span><p>Welcome to my page!</p></div>
```

Simple and effective! But again, we can't:

- Validate structure (e.g., `<p>` inside `<p>`)
- Pretty-print with indentation
- Transform (e.g., add CSS classes)

## Improving Type Safety: Phantom Types

We can use phantom types to enforce HTML structure:

```haskell
data HTML a where
  Tag :: String -> HTML a -> HTML a
  Text :: String -> HTML a
  Seq :: HTML a -> HTML a -> HTML a

-- Type-level tags
data Block
data Inline

-- Smart constructors with types
div :: HTML Block -> HTML Block
p :: HTML Block -> HTML Block
span :: HTML Inline -> HTML Inline

-- This won't compile!
-- invalid = p (p (text "nested blocks"))  -- Type error!
```

## Live Coding: Form Validation DSL

Let's build a DSL for validating form inputs:

```haskell
type Validator a = a -> Either String a

-- Primitives
nonEmpty :: Validator String
nonEmpty s = if null s then Left "Cannot be empty" else Right s

minLength :: Int -> Validator String
minLength n s =
  if length s < n
  then Left $ "Must be at least " ++ show n ++ " characters"
  else Right s

-- Combinators
(<&>) :: Validator a -> Validator a -> Validator a
(v1 <&> v2) x = v1 x >>= v2

-- Example
usernameValidator :: Validator String
usernameValidator = nonEmpty <&> minLength 3
```

## Exercise: Arithmetic DSL (Shallow)

Implement a shallow embedding for arithmetic expressions:

```haskell
type Expr = Int

lit :: Int -> Expr
lit = undefined

add :: Expr -> Expr -> Expr
add = undefined

mul :: Expr -> Expr -> Expr
mul = undefined

eval :: Expr -> Int
eval = undefined

-- Should work:
-- eval (add (lit 2) (mul (lit 3) (lit 4)))  ==> 14
```

// Solution:
// type Expr = Int
// lit = id
// add = (+)
// mul = (*)
// eval = id

## LECTURE 3: DEEP EMBEDDING & INTERPRETERS

## What is Deep Embedding?

In deep embedding, a DSL program is represented as an *abstract syntax tree* (AST)—a data structure.

**Key idea**: DSL operations build tree nodes. Interpretation is separate.

Example: Arithmetic expressions

```haskell
data Expr
  = Lit Int
  | Add Expr Expr
  | Mul Expr Expr

-- Builds an AST, doesn't compute yet
example :: Expr
example = Add (Lit 2) (Mul (Lit 3) (Lit 4))
```

## Why Deep Embedding?

Deep embedding gives us *reification*—the program is a first-class value we can:

1. **Inspect**: Examine structure
2. **Transform**: Optimize, simplify
3. **Interpret**: Multiple ways (eval, compile, pretty-print)
4. **Serialize**: Save/load programs
5. **Analyze**: Check properties

The tradeoff: More boilerplate and potential overhead.

## Building an AST: Arithmetic DSL

```haskell
data Expr
  = Lit Int
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  deriving (Show, Eq)

-- Smart constructors
lit :: Int -> Expr
lit = Lit

add, sub, mul, divide :: Expr -> Expr -> Expr
add = Add
sub = Sub
mul = Mul
divide = Div
```

## Writing an Interpreter

An interpreter traverses the AST and computes results:

```haskell
eval :: Expr -> Int
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Sub e1 e2) = eval e1 - eval e2
eval (Mul e1 e2) = eval e1 * eval e2
eval (Div e1 e2) = eval e1 `div` eval e2
```

Example:

```haskell
expr = add (lit 2) (mul (lit 3) (lit 4))
eval expr  -- ==> 14
```

## Multiple Interpretations

The power of deep embedding: multiple interpreters!

```haskell
-- Pretty printer
pretty :: Expr -> String
pretty (Lit n) = show n
pretty (Add e1 e2) = "(" ++ pretty e1 ++ " + " ++ pretty e2 ++ ")"
pretty (Sub e1 e2) = "(" ++ pretty e1 ++ " - " ++ pretty e2 ++ ")"
pretty (Mul e1 e2) = "(" ++ pretty e1 ++ " * " ++ pretty e2 ++ ")"
pretty (Div e1 e2) = "(" ++ pretty e1 ++ " / " ++ pretty e2 ++ ")"

-- Example
pretty expr  -- ==> "(2 + (3 * 4))"
```

## Optimization via Transformation

We can optimize by transforming the AST:

```haskell
optimize :: Expr -> Expr
optimize (Add (Lit 0) e) = optimize e       -- 0 + e = e
optimize (Add e (Lit 0)) = optimize e       -- e + 0 = e
optimize (Mul (Lit 0) e) = Lit 0            -- 0 * e = 0
optimize (Mul e (Lit 0)) = Lit 0            -- e * 0 = 0
optimize (Mul (Lit 1) e) = optimize e       -- 1 * e = e
optimize (Mul e (Lit 1)) = optimize e       -- e * 1 = e
optimize (Add e1 e2) = Add (optimize e1) (optimize e2)
optimize (Mul e1 e2) = Mul (optimize e1) (optimize e2)
optimize e = e
```

```haskell
expr2 = mul (add (lit 0) (lit 5)) (lit 1)
optimize expr2  -- ==> Lit 5
```

## Constant Folding

Another optimization: evaluate constant subexpressions:

```haskell
constFold :: Expr -> Expr
constFold (Add (Lit a) (Lit b)) = Lit (a + b)
constFold (Sub (Lit a) (Lit b)) = Lit (a - b)
constFold (Mul (Lit a) (Lit b)) = Lit (a * b)
constFold (Div (Lit a) (Lit b)) = Lit (a `div` b)
constFold (Add e1 e2) = Add (constFold e1) (constFold e2)
constFold (Sub e1 e2) = Sub (constFold e1) (constFold e2)
constFold (Mul e1 e2) = Mul (constFold e1) (constFold e2)
constFold (Div e1 e2) = Div (constFold e1) (constFold e2)
constFold e = e
```

```haskell
expr3 = add (mul (lit 2) (lit 3)) (lit 4)
constFold expr3  -- ==> Add (Lit 6) (Lit 4)
constFold (constFold expr3)  -- ==> Lit 10
```

## Combining Transformations

Transformations compose naturally:

```haskell
fullyOptimize :: Expr -> Expr
fullyOptimize = fixpoint (constFold . optimize)
  where
    fixpoint f x = let x' = f x
                   in if x' == x then x else fixpoint f x'
```

```haskell
expr4 = add (mul (add (lit 0) (lit 2)) (lit 3)) (lit 0)
fullyOptimize expr4  -- ==> Lit 6
```

## Case Study: Query DSL (Deep)

Let's revisit our query DSL with deep embedding:

```haskell
data Query a b where
  SelectAll :: Query a a
  SelectMap :: (a -> b) -> Query a b
  Where :: (a -> Bool) -> Query a a
  Compose :: Query b c -> Query a b -> Query a c

-- Smart constructors
selectAll :: Query a a
selectAll = SelectAll

selectMap :: (a -> b) -> Query a b
selectMap = SelectMap

whereFilter :: (a -> Bool) -> Query a a
whereFilter = Where

(<|>) :: Query b c -> Query a b -> Query a c
(<|>) = Compose
```

## Query DSL: Interpreter

```haskell
runQuery :: Query a b -> [a] -> [b]
runQuery SelectAll xs = xs
runQuery (SelectMap f) xs = map f xs
runQuery (Where p) xs = filter p xs
runQuery (Compose q1 q2) xs = runQuery q1 (runQuery q2 xs)
```

Now we can do more than just run queries!

## Query DSL: Optimization

```haskell
optimizeQuery :: Query a b -> Query a b
optimizeQuery (Compose (SelectMap f) (SelectMap g)) =
  SelectMap (f . g)  -- Fuse consecutive maps
optimizeQuery (Compose (Where p) (Where q)) =
  Where (\x -> p x && q x)  -- Fuse consecutive filters
optimizeQuery (Compose SelectAll q) = q  -- Remove identity
optimizeQuery (Compose q SelectAll) = q
optimizeQuery (Compose q1 q2) =
  Compose (optimizeQuery q1) (optimizeQuery q2)
optimizeQuery q = q
```

## Query DSL: SQL Generation

```haskell
toSQL :: Query a b -> String
toSQL SelectAll = "SELECT *"
toSQL (SelectMap _) = "SELECT (mapped columns)"  -- Simplified
toSQL (Where _) = "WHERE (predicate)"
toSQL (Compose q1 q2) = toSQL q2 ++ " " ++ toSQL q1

-- More realistic version would track column names, build proper SQL
```

Deep embedding enables code generation!

## GADTs for Type-Safe DSLs

Generalized Algebraic Data Types (GADTs) allow more precise typing:

```haskell
data Expr a where
  LitInt :: Int -> Expr Int
  LitBool :: Bool -> Expr Bool
  Add :: Expr Int -> Expr Int -> Expr Int
  Mul :: Expr Int -> Expr Int -> Expr Int
  Eq :: Expr Int -> Expr Int -> Expr Bool
  If :: Expr Bool -> Expr a -> Expr a -> Expr a
```

This prevents invalid expressions like `Add (LitBool True) (LitInt 5)` at compile time!

## Type-Safe Interpreter with GADTs

```haskell
eval :: Expr a -> a
eval (LitInt n) = n
eval (LitBool b) = b
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2
eval (Eq e1 e2) = eval e1 == eval e2
eval (If cond e1 e2) = if eval cond then eval e1 else eval e2
```

The return type varies with the expression type—type safe!

```haskell
example1 :: Expr Int
example1 = If (Eq (LitInt 2) (LitInt 2)) (LitInt 10) (LitInt 20)

eval example1  -- ==> 10 :: Int
```

## Case Study: Circuit DSL

```haskell
data Circuit a b where
  Identity :: Circuit a a
  Fan :: Circuit a (a, a)
  Swap :: Circuit (a, b) (b, a)
  Beside :: Circuit a b -> Circuit c d -> Circuit (a, c) (b, d)
  Compose :: Circuit b c -> Circuit a b -> Circuit a c

  -- Primitive gates
  And :: Circuit (Bool, Bool) Bool
  Or :: Circuit (Bool, Bool) Bool
  Not :: Circuit Bool Bool
```

## Circuit DSL: Smart Constructors

```haskell
-- Combinators
(>>>) :: Circuit a b -> Circuit b c -> Circuit a c
(>>>) = flip Compose

(***) :: Circuit a b -> Circuit c d -> Circuit (a, c) (b, d)
(***) = Beside

-- Example: XOR gate
xor :: Circuit (Bool, Bool) Bool
xor =
  (Fan *** Identity) >>>           -- ((a,a), b)
  (Swap *** Identity) >>>          -- ((a,a), b) -> ((a,b), a)
  (Beside And Identity) >>>        -- (a && b, a)
  -- ... (full XOR would be more complex)
```

## Circuit DSL: Simulation

```haskell
simulate :: Circuit a b -> a -> b
simulate Identity x = x
simulate Fan x = (x, x)
simulate Swap (x, y) = (y, x)
simulate (Beside c1 c2) (x, y) = (simulate c1 x, simulate c2 y)
simulate (Compose c1 c2) x = simulate c1 (simulate c2 x)
simulate And (x, y) = x && y
simulate Or (x, y) = x || y
simulate Not x = not x
```

## Circuit DSL: Visualization

```haskell
-- Generate graphviz DOT format
visualize :: Circuit a b -> String
visualize Identity = "id"
visualize Fan = "fan"
visualize And = "AND"
visualize (Compose c1 c2) =
  visualize c2 ++ " -> " ++ visualize c1
-- ... etc

-- Could also generate circuit diagrams, SVG, etc.
```

## Live Coding: State Machine DSL

```haskell
data FSM state input output where
  State :: state -> FSM state input output
  Transition :: (state -> input -> state) -> FSM state input output
  Output :: (state -> output) -> FSM state input output
  Compose :: FSM s1 i1 o1 -> FSM s2 i2 o2 -> FSM (s1, s2) (i1, i2) (o1, o2)

-- Example: Traffic light
data Light = Red | Yellow | Green
data Event = Tick | Emergency

trafficLight :: FSM Light Event String
trafficLight = undefined  -- Exercise!
```

## Exercise: Expression Simplifier

Implement an expression simplifier that applies algebraic rules:

```haskell
data Expr
  = Var String
  | Lit Int
  | Add Expr Expr
  | Mul Expr Expr

-- Implement these rules:
-- x + 0 = x
-- 0 + x = x
-- x * 1 = x
-- 1 * x = x
-- x * 0 = 0
-- 0 * x = 0

simplify :: Expr -> Expr
simplify = undefined
```

// Solution similar to optimize function shown earlier, but with Var case:
// simplify (Var x) = Var x
// simplify (Lit n) = Lit n
// simplify (Add (Lit 0) e) = simplify e
// simplify (Add e (Lit 0)) = simplify e
// -- ... etc

## Compiling DSLs

Deep embedding enables compilation to different targets:

```haskell
-- Compile arithmetic expressions to stack machine
compile :: Expr -> [Instruction]
compile (Lit n) = [Push n]
compile (Add e1 e2) = compile e1 ++ compile e2 ++ [AddOp]
compile (Mul e1 e2) = compile e1 ++ compile e2 ++ [MulOp]

data Instruction = Push Int | AddOp | MulOp

-- Stack machine interpreter
runStack :: [Instruction] -> [Int] -> Int
runStack [] [x] = x
runStack (Push n : rest) stack = runStack rest (n : stack)
runStack (AddOp : rest) (a:b:stack) = runStack rest (b + a : stack)
runStack (MulOp : rest) (a:b:stack) = runStack rest (b * a : stack)
```

## Compilation Example

```haskell
expr = add (lit 2) (mul (lit 3) (lit 4))

compiled = compile expr
-- ==> [Push 2, Push 3, Push 4, MulOp, AddOp]

result = runStack compiled []
-- ==> 14
```

Compilation enables efficient execution or code generation for other platforms!

## Free Monads and DSLs

*Free monads* provide another approach to deep embedding:

```haskell
data Free f a
  = Pure a
  | Free (f (Free f a))

-- Example: Command DSL
data Command next
  = Print String next
  | Read (String -> next)
  deriving Functor

type Program = Free Command

-- Smart constructors
printLine :: String -> Program ()
printLine s = Free (Print s (Pure ()))

readLine :: Program String
readLine = Free (Read Pure)
```

## Free Monad Interpreters

```haskell
runProgram :: Program a -> IO a
runProgram (Pure x) = return x
runProgram (Free (Print s next)) = putStrLn s >> runProgram next
runProgram (Free (Read f)) = getLine >>= runProgram . f

-- Example program
example :: Program ()
example = do
  printLine "What's your name?"
  name <- readLine
  printLine $ "Hello, " ++ name

-- Run it
main = runProgram example
```

Free monads separate program description from interpretation!

## When to Use Free Monads

Free monads are useful for:

- Testing (mock interpreters)
- Multiple backends (different interpreters)
- Analysis (inspect program structure)

But they add complexity:

- More boilerplate
- Performance overhead
- Steeper learning curve

Use when you need the flexibility; prefer simpler approaches otherwise.

## Comparing Embedding Styles

| Feature               | Shallow   | Deep     | Free Monad |
| --------------------- | --------- | -------- | ---------- |
| Implementation        | Simple    | Moderate | Complex    |
| Inspection            | No        | Yes      | Yes        |
| Optimization          | No        | Yes      | Yes        |
| Multiple Interpreters | No        | Yes      | Yes        |
| Overhead              | Low       | Medium   | High       |
| Composability         | Excellent | Good     | Excellent  |

## Best Practices for DSL Design

1. **Start simple**: Begin with shallow embedding, refactor if needed

2. **Type safety first**: Use types to prevent invalid programs

3. **Composition is key**: Make operations composable

4. **Documentation**: Clear examples and domain explanation

5. **Error messages**: Provide helpful feedback

6. **Performance**: Profile before optimizing

7. **Evolution**: Design for future extensions

## DSL Implementation Checklist

When implementing a DSL, consider:

- [ ] What operations does the domain need?
- [ ] What compositions make sense?
- [ ] What invariants should types enforce?
- [ ] Do you need multiple interpretations?
- [ ] Can you provide good error messages?
- [ ] How will users learn the DSL?
- [ ] What are the performance requirements?

## Cross-Language Comparison: DSLs

**Scala**: Object-oriented DSLs with traits and implicits

```scala
// Builder pattern DSL
val person = Person.builder
  .name("Alice")
  .age(30)
  .build()
```

**Ruby**: Metaprogramming for DSLs

```ruby
# Rake DSL
task :deploy do
  sh "git push heroku master"
end
```

**Rust**: Macros for DSLs

```rust
// HTML macro
html! {
  div { p { "Hello" } }
}
```

## Summary: Domain-Specific Languages

**Internal DSLs**: Embedded in host language

- Leverage host features
- Faster to develop

**Shallow Embedding**: Represent by denotation

- Simple, direct
- Limited inspection

**Deep Embedding**: Represent as AST

- Multiple interpretations
- Optimization, transformation, analysis

**Key Principles**: Composability, type safety, domain-driven design

DSLs let you express solutions in domain terms—a powerful tool in your programming toolbox!

## Further Exploration

Topics we didn't cover (but you might explore):

- **Parser combinators**: External DSLs with Parsec/Megaparsec
- **Template Haskell**: Compile-time metaprogramming
- **Type-level programming**: Encoding more invariants
- **Effect systems**: Managing side effects in DSLs
- **Tagless final**: Alternative to shallow/deep

DSLs are a deep topic with endless possibilities!

## Final Exercise: Design Your Own DSL

Design a DSL for a domain of your choice:

1. Choose a domain (e.g., recipes, music, finite automata, neural networks)
2. Identify core operations and compositions
3. Sketch types and functions
4. Decide: shallow or deep embedding?
5. Implement a prototype
6. Test with realistic examples

Share your designs—DSL design is both art and science!
