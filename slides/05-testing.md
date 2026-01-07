---
title: "Testing"
sub_title: "CS 340: Programming Patterns and Paradigms"
author: "Michael Lee <lee@iit.edu>"
---

# Agenda

- Definition & Motivation
- Test-Driven Development (TDD)
- Approaches to Testing
  - Example-based testing
  - Property-based testing
- TDD: A Complete Example
- Test coverage

---

# What is Testing?

A *test* verifies that some aspect of a system works to specification.

## Why test?

- Catch bugs early
- Prevent future regressions
- Document expected behavior
- Build confidence in correctness

---

# Testing Frameworks

Testing frameworks are software libraries that provide:

- Structured ways to specify different types of tests
- Automatic test discovery and execution
- Clear reporting of failures
- Coverage analysis

---

# Test-Driven Development (TDD)

Write tests *first*, then write code to make them pass.

TDD workflow:

1. Write a test for desired functionality (test fails - "red")
2. Write minimal code to make the test pass ("green")
3. Refactor code while keeping tests passing
4. Repeat

---

## Benefits of TDD

- Forces you to think about requirements and API design upfront
- Ensures all code is testable
- Provides fast feedback loop
- Prevents you from doing more than is necessary

---

# Approaches to Testing

*Static testing* - Performed by the compiler

- Types and type checking catch errors before runtime

*Unit testing* - Testing individual functions/modules

- *Example-based*: Explicit inputs → expected outputs
- *Property-based*: General properties that should hold for all inputs

*Integration/System testing* - Testing components working together

---

## Example-based tests with Hspec

`Hspec` helps us write test specifications in Haskell:

```haskell
import Test.Hspec
import Test.QuickCheck

mySpec :: Spec
mySpec = 
  describe "myFunction" $ do  -- `describe` groups tests
    it "handles the base case" $  -- `it` denotes a test case
      myFunction [] `shouldBe` 0 
    it "works on known examples" $ do
      myFunction [1,2,3] `shouldBe` 6  -- `shouldBe` et al set
      myFunction [5] `shouldBe` 5      --  test expectations
    it "satisfies important properties" $
      property prop_myFunction  -- `property` denotes a 
                                --  property-based test
```

---

## Pros/Cons of Example-Based Testing

**Pros:**

- Clear, easy to understand, good for edge cases

**Cons:**

- Only tests the examples you think of
- Can miss edge cases
- Tedious to write comprehensive test suites (AI can help!)
- Hard to test functions with large input spaces

---

# Property-Based Testing

Instead of individual examples, we specify *properties* that should hold for
*all* inputs, and the test framework *generates* tests for us.

```haskell
-- Property: reversing twice gives back the original
prop_reverseReverse :: [Int] -> Bool
prop_reverseReverse xs = reverse (reverse xs) == xs
```

QuickCheck (a Haskell property-based testing library) will:

1. Generate 100 random `[Int]` values
2. Check the property for each
3. Report any failures
4. Try to "shrink" failures to minimal counterexamples

---

# Why Property-Based Testing?

Explores the input space more *thoroughly*:

- Tests (edge) cases you didn't think of
- Tests combinations of inputs

Documents behavior at a *higher level*:

- Properties capture the "essence" of a function
- Less brittle and easier to maintain than specific examples

*Complements* example-based testing:

- Use examples for specific edge cases and clarity
- Use properties for general correctness

---

# What Makes a Good Property?

Good properties are:

1. *General*: Should hold for all valid inputs, not just some
2. *Meaningful*: Should actually test something important
3. *Independent*: Shouldn't depend on implementation details

---

# Common Property Patterns

- *Inverse functions*: `f (g x) == x`
- *Invariants*: Something that doesn't change (e.g., `length` after `sort`)
- *Idempotence*: Applying twice = applying once (e.g., `f (f x) == f x`)
- *Commutativity*: Order doesn't matter (e.g., `x + y == y + x`)
- *Reference implementation*: Compare against a known-correct implementation

---

## E.g., Inverse Functions

Functions that "undo" each other should round-trip:

```haskell
-- reverse is its own inverse
prop_reverseInverse :: [Int] -> Bool
prop_reverseInverse xs = reverse (reverse xs) == xs

-- Converting back and forth between representations
prop_c2f2c :: Double -> Bool
prop_c2f2c c = abs (f2c (c2f c) - c) < 0.0001
```

This is one of the most common and useful property patterns!

---

## E.g., Invariants

Some properties of the output shouldn't change:

```haskell
-- Sorting preserves length
prop_sortLength :: [Int] -> Bool
prop_sortLength xs = length (sort xs) == length xs

-- Sorting preserves elements (just reorders)
prop_sortPreserves :: [Int] -> Bool
prop_sortPreserves xs = sort (sort xs) == sort xs
```

---

## E.g., Algebraic Laws

Test mathematical properties and relationships:

```haskell
-- Empty list is identity for append
prop_appendIdentity :: [Int] -> Bool
prop_appendIdentity xs = xs ++ [] == xs && [] ++ xs == xs

-- Commutativity of addition
prop_sumCommutes :: [Int] -> Bool
prop_sumCommutes xs = sum xs == sum (reverse xs)
```

---

# TDD: A Complete Example

Let's walk through developing a list function using TDD:

**Goal:** Write `removeAt` — remove an element at a given index

```haskell
removeAt :: Int -> [a] -> [a]
```

Examples:

- `removeAt 0 [1,2,3]` → `[2,3]`
- `removeAt 1 [1,2,3]` → `[1,3]`
- `removeAt 2 [1,2,3]` → `[1,2]`

---

## Step 1: Write Property-Based Tests First

Before writing any code, let's think: what should always be true?

**Property:** Removing an element should shorten the list by 1

```haskell
prop_removeAtLength :: Int -> [Int] -> Bool
prop_removeAtLength n xs = 
  length (removeAt n xs) == length xs - 1
```

<!-- pause -->

**Problem:** What if `n` is negative? Or `n >= length xs`?

<!-- pause -->

We need a *precondition* — a requirement that must be true for the property to
be meaningful.

---

### Conditional Properties with `==>`

QuickCheck's `==>` operator lets us add preconditions:

```haskell
prop_removeAtLength :: Int -> [Int] -> Property
prop_removeAtLength n xs = 
  (n >= 0 && n < length xs) ==> 
    length (removeAt n xs) == length xs - 1
```

<!-- pause -->

Left side of `==>` is the precondition; Right side is the property to test

- `==>` returns a `Property`

---

### How `==>` Works

```haskell
prop_removeAtLength :: Int -> [Int] -> Property
prop_removeAtLength n xs = 
  (n >= 0 && n < length xs) ==> 
    length (removeAt n xs) == length xs - 1
```

<!-- pause -->

When QuickCheck runs this:

1. Generates random `n` and `xs`
2. Checks if `n >= 0 && n < length xs`
3. If `False`: discards this test case and tries another
4. If `True`: checks the property `length (removeAt n xs) == length xs - 1`

---

## Step 2: Write Our Test Spec

```haskell
removeAtSpec :: Spec
removeAtSpec = 
  describe "removeAt" $ do
    it "shortens the list by 1" $
      property prop_removeAtLength
```

<!-- pause -->

This test will fail because we haven't written `removeAt` yet!

---

## Step 3: Initial Implementation (Buggy!)

First attempt — seems reasonable:

```haskell
removeAt :: Int -> [a] -> [a]
removeAt n xs = take n xs ++ drop n xs
```

<!-- pause -->

Logic:

- `take n xs` gets elements before index n
- `drop n xs` skips to index n
- Concatenate them together

<!-- pause -->

Seems right... let's run the tests!

---

## Step 4: Properties Fail Immediately!

```
removeAt
  shortens the list by 1
    *** Failed! Falsified (after 5 tests and 2 shrinks):
    1
    [0,1]
```

<!-- pause -->

The property caught a bug right away!

<!-- pause -->

QuickCheck:

- Generated lots of test cases
- Found one that failed (maybe `removeAt 3 [2,5,1,8,0,9,4]`)
- **Shrunk** it to *minimal counterexample*: `n=1`, `xs=[0,1]`

---

## Step 5: Understanding the Failure

The minimal counterexample is `removeAt 1 [0,1]`:

```haskell
removeAt 1 [0,1]
  = take 1 [0,1] ++ drop 1 [0,1]
  = [0] ++ [1]
  = [0,1]
```

<!-- pause -->

- Original list: `[0,1]` has length 2
- After "removing": `[0,1]` has length 2
- Expected: length 1

<!-- pause -->

The bug is clear: `drop 1` keeps the element at index 1, it doesn't skip it!

<!-- pause -->

**We need:** `drop (n+1)` not `drop n`

---

### The Power of Shrinking

Without shrinking, we might have seen:

```
*** Failed! Falsified (after 5 tests):
3
[2,5,1,8,0,9,4]
```

Debugging this is harder!

<!-- pause -->

With shrinking:

```
*** Failed! Falsified (after 5 tests and 2 shrinks):
1
[0,1]
```

The minimal example makes the bug obvious!

---

## Step 6: Fix the Bug

Correct implementation:

```haskell
removeAt :: Int -> [a] -> [a]
removeAt n xs = take n xs ++ drop (n+1) xs
```

<!-- pause -->

Now let's verify:

```haskell
removeAt 1 [0,1]
  = take 1 [0,1] ++ drop 2 [0,1]
  = [0] ++ []
  = [0]
```

<!-- pause -->

Length is now 1 — correct!

---

## Step 7: Property Passes!

```
removeAt
  shortens the list by 1
    +++ OK, passed 100 tests; 456 discarded.
```

<!-- pause -->

"456 discarded" means QuickCheck tried many random `n` and `xs` pairs where `n`
wasn't a valid index. It still found 100 valid cases to test.

<!-- pause -->

But discarding 456 tests to find 100 valid ones is wasteful...

Can we do better?

---

## Step 8: Better Test Generation with `forAll`

<!-- pause -->

Solution: Use `forAll` to generate only valid test cases:

```haskell
prop_removeAtLength' :: Property
prop_removeAtLength' = 
  forAll (listOf1 arbitrary) $ \xs ->
  forAll (choose (0, length xs - 1)) $ \n ->
    length (removeAt n xs) == length xs - 1
```

No precondition needed — all generated cases are valid!

---

### How `forAll` Works

```haskell
forAll generator $ \value -> property
```

<!-- pause -->

`forAll` takes two arguments:

1. A *generator* that produces random values
2. A *function* from the generated value to a property

<!-- pause -->

Common generators:

- `arbitrary` — default generator for any type
- `choose (low, high)` — random number in range
- `listOf1 arbitrary` — non-empty list
- `elements [...]` — random element from a list

---

### Our Usage of `forAll`

```haskell
prop_removeAtLength' :: Property
prop_removeAtLength' = 
  forAll (listOf1 arbitrary) $ \xs ->
  forAll (choose (0, length xs - 1)) $ \n ->
    length (removeAt n xs) == length xs - 1
```

<!-- pause -->

Reading this:

1. Generate a non-empty list, call it `xs`
2. Generate a valid index in [0, length-1], call it `n`
3. Check that `length (removeAt n xs) == length xs - 1`

---

### `forAll` Result and Summary

Result: No discarded tests!

```
removeAt
  shortens the list by 1
    +++ OK, passed 100 tests.
```

Both `==>` and `forAll` give us control over test cases

- `==>` is simple but may discard many cases
- `forAll` generates only valid test cases, and gives us full control over test
  distribution

---

## Step 9: Add More Properties

**Property 2:** The element at index n should be gone

```haskell
prop_removeAtCorrect :: Property
prop_removeAtCorrect = 
  forAll (listOf1 arbitrary) $ \xs ->
  forAll (choose (0, length xs - 1)) $ \n ->
    let removed = removeAt n xs
        before = take n xs
        after = drop (n+1) xs
    in removed == before ++ after
```

<!-- pause -->

This checks that we're actually removing the right element and keeping
everything else in order.

---

## Step 10: Add Example-Based Tests

Now that our properties pass, let's add some example tests for documentation:

```haskell
removeAtSpec :: Spec
removeAtSpec = 
  describe "removeAt" $ do
    it "shortens the list by 1" $
      property prop_removeAtLength'
    
    it "removes the correct element" $
      property prop_removeAtCorrect
    
    it "removes from single-element list" $
      removeAt 0 [42] `shouldBe` ([] :: [Int])
    
    it "removes first element" $
      removeAt 0 [1,2,3] `shouldBe` [2,3]
    
    it "removes last element" $
      removeAt 2 [1,2,3] `shouldBe` [1,2]
```

---

# Property-First TDD Workflow

1. **Think about invariants:** What should always be true?
2. **Write properties:** Express those invariants as tests
3. **Implement:** Write code to satisfy properties
4. **Debug:** Let shrinking guide you to bugs
5. **Add examples:** Document specific cases

<!-- pause -->

This is more powerful than:

1. Write examples
2. Implement
3. Hope you covered all cases

---

# Test Coverage

How much of our code is actually tested?

```bash
stack test --coverage
```

Generates a report showing:

- Which functions are called
- Which patterns/branches are exercised
- Which code is unreachable (dead code)

---

# Coverage Example

After running tests with coverage, you might see:

```
 50% expressions used (42/84)
 66% boolean coverage (4/6)
 37% guards (3/8)
 75% if conditions (3/4)
100% function coverage (5/5)
```

<!-- pause -->

This tells you:

- Some expressions never evaluated
- Some guards never taken
- But all functions are at least called

<!-- pause -->

Use this to identify untested code paths!

---

# Best Practices

**Start simple:**

- Begin with inverse functions and invariants
- Add more complex properties as you gain experience

<!-- pause -->

**Combine approaches:**

- Use examples for specific edge cases
- Use properties for general correctness

<!-- pause -->

**Let failures guide you:**

- Shrunk counterexamples reveal the essence of bugs
- Don't ignore what QuickCheck finds!

---

# Best Practices (cont.)

**Think about properties while coding:**

- "What should always be true?"
- "What are the invariants?"
- "Is there a reference implementation?"

<!-- pause -->

**Write tests first (TDD):**

- Properties document expected behavior
- Tests catch bugs early
- Refactoring is safer

<!-- pause -->

**Aim for high coverage:**

- But don't sacrifice property quality for coverage numbers
- Meaningful properties naturally increase coverage
