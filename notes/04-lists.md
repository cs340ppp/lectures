# Lists
## CS 340: Programming Patterns and Paradigms
Michael Lee <lee@iit.edu>

## Agenda

- The List Type & List Constructors
- Functions Over Lists & Structural Recursion
- "Infinite" Lists & Lazy Evaluation
- List Comprehensions

## The List Type

A list is a *homogeneous*, *ordered* collection of zero or more values

- I.e., all elements must be of the same (possibly polymorphic) type

A list has type `[t]`, where `t` is the type of its elements. E.g.,

- `[Int]` denotes a list of `Int`s

- `[Char]` denotes a list of `Char`s (aka a `String`)

- `[(Char, Bool)]` denotes a list of `(Char, Bool)` pairs

- `[[Char]]` denotes a list of list of `Char`s (a list of `String`s)

- `[a]` denotes a list containing elements of polymorphic type `a`

## List Constructors

There are two *value constructors* for the list type:

- `[]`, which constructs an empty list

- `:` ("cons"), which takes a value of type `a` (the "head") and a list of type
  `[a]` (the "tail"), and constructs a list by prepending the head to the tail

  - i.e., `(:) :: a -> [a] -> [a]`

  - `:` is *right-associative*; e.g., 1 : 2 : [] === 1 : (2 : [])

### E.g., List Construction

```haskell
intList :: [Int]
intList = 1 : 2 : 3 : 4 : 5 : []

str1, str2 :: [Char]
str1 = 'w' : 'o' : 'r' : 'l' : 'd' : []
str2 = 'h' : 'e' : 'l' : 'l' : 'o' : ' ' : str1

vowels :: [(Char, Bool)]
vowels = ('a', True) : ('b', False) : ('e', True) : []

counts :: [[Int]]
counts = (1 : []) : (1 : 2 : []) : (1 : 2 : 3 : []) : []
```

### Syntactic Sugar for List Construction

Instead of using `:` to manually build lists element-by-element, Haskell
provides *syntactic sugar* to simplify/prettify code:

- `x : y : z : []` === `[x, y, z]`

For lists of characters (`[Char]` aka `String`), there is another form:

- `'a' : 'b' : 'c' : []` === `['a', 'b', 'c']` === `"abc"`

Under the hood, the "sugared" forms are translated to the long form for us!

- It is still important/useful to understand how '`:`' works

### E.g., List Construction with Syntactic Sugar

```haskell
intList :: [Int]
intList = [1, 2, 3, 4, 5]

str1, str2 :: [Char]
str1 = "world"
str2 = 'h' : 'e' : 'l' : 'l' : 'o' : ' ' : str1

vowels :: [(Char, Bool)]
vowels = [('a', True), ('b', False), ('e', True)]

counts :: [[Int]]
counts = [[1], [1,2], [1,2,3]]
```

## Functions on Lists

Just like tuples, lists can be deconstructed using pattern matching:

- `[]` matches the empty list

- `x:xs` matches a non-empty list, binding `x` to the head and `xs` to the tail

```haskell
-- Pattern match on empty list
isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty _  = False

-- Pattern match on cons
firstTwo :: [a] -> (a, a)
firstTwo (x:y:_) = (x, y)
```

This is the same pattern matching we saw with tuples and other values—just
applied to list constructors!

## Functions Over Lists

A function that takes a list as an argument can pattern match on values
constructed with `[]` and `:`. E.g.,

```haskell
head :: [a] -> a
head (x:_) = x

tail :: [a] -> [a]
tail (_:xs) = xs

null :: [a] -> Bool
null [] = True
null _  = False
```

- Note that `head` and `tail` are partial functions by design

## Structural Recursion

Haskell lacks loops, so to process multiple elements in a list, we typically
reach for *recursion*. E.g.,

```haskell
length :: [a] -> Int
length [] = 0
length (_:xs) = 1 + length xs
```

We incrementally deconstruct the list over successive recursive calls; this is
is an example of *structural recursion*

- `[]` establishes the *base case*: recursion stops here

- `:` is destructured via pattern matching

  - We account for the first element (`1 +`), and delegate handling the rest of
    the list (`xs`) to a recursive call

### Tracing `length`

```haskell
length :: [a] -> Int
length [] = 0
length (_:xs) = 1 + length xs
```

- `length (4 : 5 : 6 : [])`

- `1 + length (5 : 6 : [])`

- `1 + (1 + length (6 : []))`

- `1 + (1 + (1 + length []))`

- `1 + (1 + (1 + 0))`

- `1 + (1 + 1))`

- `1 + 2`

- `3`

Intuitively, we recurse down to the base case (here, the end of the list), then
compute the final result "on the way out" of the recursive calls.

### E.g., More Structural Recursion

Very similar to `length`, but we add the first element to the result.

```haskell
sum :: Num a => [a] -> a
sum [] = 0
sum (n:ns) = n + sum ns
```

- `sum [1, 2, 3, 4, 5]` => `15`

The base case needn't be the end of the list! Here, we chase down the last
element (if it exists).

```haskell
last :: [a] -> a
last [x] = x
last (_:xs) = last xs
last [] = error "empty list"
```

- `last "abcd"` => `'d'`

Note that we can pattern match on list syntactic sugar!

This demonstrates a common "filtering" pattern; we only track (`1 +`) elements
that match some criteria.

```haskell
count :: Eq a => a -> [a] -> Int
count _ [] = 0
count x (y:ys) | x == y    = 1 + count x ys
               | otherwise = count x ys
```

- `count 'l' "hello world"` => `3`

## Functions that Build Lists

We can also use recursion to build lists. E.g.,

```haskell
replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n x = x : replicate (n-1) x
```

We incrementally construct the list, where:

- `0` establishes the base case; we just return `[]`

- Otherwise, we construct a list (using `:`) from one copy of `x` and the list
  returned by a recursive call

### Tracing `replicate`

```haskell
replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n x = x : replicate (n-1) x
```

- `replicate 3 'a'`

- `'a' : replicate 2 'a'`

- `'a' : 'a' : replicate 1 'a'`

- `'a' : 'a' : 'a' : replicate 0 'a'`

- `'a' : 'a' : 'a' : []`

### E.g., More Functions that Build Lists

We can generate an ordered sequence of elements using `succ` (from `Ord`):

```haskell
enumFromTo :: (Ord a, Enum a) => a -> a -> [a]
enumFromTo x y | x <= y    = x : enumFromTo (succ x) y
               | otherwise = []
```

- `enumFromTo 10 15` => `[10,11,12,13,14,15]`

### E.g., Processing & Building Lists

A function can simultaneously deconstruct and construct lists. E.g.,

```haskell
keepEvens :: Integral a => [a] -> [a]
keepEvens [] = []
keepEvens (n:ns) | n `rem` 2 == 0 = n : keepEvens ns
                 | otherwise      = keepEvens ns
```

- `keepEvens [42, 11, 19, 22, 100]` => `[42,22,100]`

A useful pair of list functions are `take` and `drop`:

```haskell
take :: Int -> [a] -> [a]
take 0 _ = []
take _ [] = []
take n (x:xs) = x : take (n-1) xs

drop :: Int -> [a] -> [a]
drop 0 xs = xs
drop _ [] = []
drop n (x:xs) = drop (n-1) xs
```

- `take 5 "hello world"` => `"hello"`

- `drop 5 "hello world"` => `" world"`

## Infinite lists

Here's a function that builds a list containing copies of a given element:

```haskell
repeat :: a -> [a]
repeat x = x : repeat x
```

- It has no base case: the recursion is *unbounded*. The list is *infinite*!

- E.g., `repeat (1+2)` => `3 : 3 : 3 : 3 : 3 : ....`

In most languages, a call to a function like this would immediately *diverge*,
i.e., it would never return a usable result.

- But this is admissible in Haskell, because of *lazy evaluation*

## Lazy Evaluation

Most languages use *eager evaluation* (also called "call-by-value"):

- Function arguments are evaluated *before* the function is called

- `f (g x)` first evaluates `g x`, and passes the resulting value to `f`

Haskell uses *lazy evaluation* (also called "call-by-name"):

- Expressions aren't evaluated until their values are *needed*

- `f (g x)` only evaluates `g x` if `f` actually uses its result

### Lazy Evaluation: Thunks

When the result of an expression isn't needed immediately, Haskell creates a
*thunk* for it: a suspended computation that can be evaluated when needed.

```haskell
repeat :: a -> [a]
repeat x = x : repeat x
```

Calling `repeat 1` doesn't flesh out the (infinite) list. Instead, it just
creates a thunk representing the call: `<thunk: repeat 1>`

- Pattern matching determines how the thunk is subsequently evaluated

### E.g., Evaluating Thunks

E.g., consider the call `null (repeat 1)`, where:

```haskell
null :: [a] -> Bool
null []    = True
null (_:_) = False
```

We pattern match `<thunk: repeat 1>` against the constructors `[]` and `:`

- We evaluate `<thunk: repeat 1>` enough to determine it is a `:` value

- I.e., we evaluate to `<thunk: 1> : <thunk: repeat 1>`; this matches the
  pattern that returns `False`

E.g., consider the call `head (repeat 1)`, where:

```haskell
head :: [Int] -> Int
head (x:_) = x
```

After pattern matching `<thunk: repeat 1>` against `(x:_)`, we now have the head
`x` = `<thunk: 1>`.

- When we return x, its value will be demanded (e.g., for printing), so
  `<thunk: 1>` is evaluated to 1.

- But the tail thunk is still unevaluated!

E.g., consider the call `length (repeat 1)`, where:

```haskell
length :: [a] -> a
length [] = 0
length (_:xs) = 1 + length xs
```

Here, `<thunk: repeat 1>` keeps matching against `(_:xs)`, because `length` only
terminates when we encounter `[]`.

- `length` diverges! (It builds a separate `1 + 1 + 1 + 1 + ...` thunk that
  exhausts memory.)

E.g., consider the call `take 3 (repeat 1)`, where:

```haskell
take :: Int -> [a] -> [a]
take 0 _ = []
take _ [] = []
take n (x:xs) = x : take (n-1) xs
```

`take 3` forces evaluation of the thunk until 3 elements are taken, then
terminates its return list with `[]`.

- The rest of the infinite list is never evaluated!

#### Tracing \`take 3 (repeat 1)\`

```haskell
take 3 (repeat 1)
take 3 (<thunk: repeat 1>)
take 3 (1 : <thunk: repeat 1>)
1 : take 2 (<thunk: repeat 1>)
1 : take 2 (1 : <thunk: repeat 1>)
1 : 1 : take 1 (<thunk: repeat 1>)
1 : 1 : take 1 (1 : <thunk: repeat 1>)
1 : 1 : 1 : take 0 (<thunk: repeat 1>)
1 : 1 : 1 : []
```

### Lazy Evaluation: Key Takeaways

Functions that *work* with infinite lists:

- Only inspect the structure partially (`null`, `head`, `tail`)
- Only need a bounded prefix (`take n`, `drop n`)

Functions that *diverge* on infinite lists:

- Must traverse the entire list (`length`, `sum`, `last`)
- Try to consume all elements (`reverse`, sorting functions)

## List Comprehensions

List comprehensions provide a concise syntax for building lists, inspired by
mathematical set-builder notation.

**Set-builder notation (math):** { x^2 | x ∈ {1,2,3,4,5} }

**List comprehension (Haskell):**

```haskell
[x^2 | x <- [1,2,3,4,5]]  -- [1,4,9,16,25]
```

**Python equivalent:**

```python
[x**2 for x in [1,2,3,4,5]]
```

### List Comprehensions: Basic Syntax

Syntax: `[expr | var <- list]`

```haskell
doubles :: [Int] -> [Int]
doubles xs = [x * 2 | x <- xs]

uppercases :: String -> String
uppercases s = [toUpper c | c <- s]
```

The variable `x` (or `c`) takes on each value from the source list, and `expr`
is evaluated for each.

### List Comprehensions: Guards

Add conditions with guards (boolean predicates after a comma):

```haskell
evens :: [Int] -> [Int]
evens xs = [x | x <- xs, x `rem` 2 == 0]

vowels :: String -> String
vowels s = [c | c <- s, c `elem` "aeiouAEIOU"]
```

Only elements satisfying the guard are included in the result.

Multiple guards can be combined with commas (all must be true).

### List Comprehensions: Multiple Generators

Multiple generators create a Cartesian product:

```haskell
pairs :: [a] -> [b] -> [(a, b)]
pairs xs ys = [(x, y) | x <- xs, y <- ys]
```

```haskell
pairs [1,2] ['a','b']  -- [(1,'a'),(1,'b'),(2,'a'),(2,'b')]
```

Later generators can depend on earlier ones:

```haskell
-- All pairs where first < second
orderedPairs :: [Int] -> [(Int, Int)]
orderedPairs xs = [(x, y) | x <- xs, y <- xs, x < y]
```

### List Comprehensions: Range Notation

Haskell provides syntactic sugar for generating sequences:

```haskell
[1..10]      -- [1,2,3,4,5,6,7,8,9,10]
[1..]        -- [1,2,3,4,5,...  (infinite!)
['a'..'z']   -- "abcdefghijklmnopqrstuvwxyz"
```

These desugar to `enumFromTo` and `enumFrom`:

```haskell
[1..10]  === enumFromTo 1 10
[1..]    === enumFrom 1
```

You can also specify a step:

```haskell
[1,3..10]    -- [1,3,5,7,9]
[2,4..]      -- [2,4,6,8,... (infinite!)
```

### List Comprehensions: When to Use Them

Use list comprehensions when:

- Filtering and/or transforming a list
- The logic is straightforward and reads naturally
- The code benefits from conciseness

```haskell
[x^2 | x <- [1..10], even x]  -- Clear and concise
```

Use explicit recursion when:

- The logic is complex or involves multiple base cases
- You need fine control over the recursive structure
- Performance-critical code where you need to reason about evaluation

```haskell
-- Explicit recursion is clearer here
mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort left) (mergeSort right)
  where ...
```

Both are important tools; knowing when to use each comes with practice!
