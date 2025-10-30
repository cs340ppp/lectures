---
title: "Testing"
sub_title: "CS 340: Programming Patterns and Paradigms"
author: "Michael Lee <lee@iit.edu>"
---

# Agenda

- What is testing?
- Approaches to testing & verification
- Hspec testing framework
- Example-based tests with Hspec
- Property-based tests with QuickCheck
- Test coverage

---

# What is testing?

A *test* verifies that some aspect of a system works to specification.

<!-- pause -->

Testing tools can help:

- simplify test specification, discovery, execution, and reporting

- ensure that code changes don't break existing functionality (no regressions)

- determine code coverage (how much of the codebase is actually run)

- eliminate code "lint" (aka "dead code")

---

# Approaches to testing & verification

General strategy: Test-Driven Development (TDD)

- Write tests *first*, ensure they fail, then write code to get them to pass

- After all tests pass, any future code refactoring requires re-running tests

---

# Approaches to testing & verification

But how to write tests? (How to verify correctness?)

- *Static tests* are carried out by a compiler, which checks for syntax and
  type related errors. We write *type signatures* to help the compiler.

- *Unit tests* check that "units" of code (e.g., functions, classes) work as 
  expected. Their specification/execution is facilitated by test frameworks.

<!-- pause -->

  - *Example-based tests* explicitly declare the expected results (e.g.,
    return value, output, exception) for different inputs and/or state.
  
  - *Property-based tests* declare high-level "properties" (aka invariants) 
    that must hold true for all inputs and/or state. Specific cases are 
    automatically generated and checked.

<!-- pause -->

- *Formal verification* may be done at a higher level of abstraction. It is
  typically done by a theorem prover, which checks for logical errors by
  proving that the program satisfies a set of logical properties.

---

# Hspec testing framework

Hspec gives us a way to specify tests in a human-legible way:

```haskell
someSpec :: Spec
someSpec = 
  describe "someFunc" $ do
    it "fulfills some expectation ..." $
      pendingWith "Need to flesh out this test"
    it "fulfills some other expectation ..." $
      pending
```

<!-- pause -->

- Run a `Spec` using `hspec`.

- Hspec supports both unit tests and property-based tests

- `stack test` will run "test/Spec.hs", which will automatically discover
  all "*Spec.hs" files in the "test" directory and run their "spec" functions

---

# Example-based tests with Hspec

Hspec provides various functions for creating `Expectations`:

- `shouldBe` / `shouldNotBe`
- `shouldSatisfy` / `shouldNotSatisfy`
- `shouldMatchList` / `shouldNotMatchList`
- `shouldThrow` / `shouldNotThrow`

---

# Example-based tests with Hspec

E.g., let's write a specification for `c2k`, `c2f`, `f2c`:

```haskell
c2k :: (Ord a, Floating a) => a -> a
c2k c | c >= 0 = c + 273.15
      | otherwise = error "Temperature below absolute zero"

c2f :: Floating a => a -> a
c2f c = c * 9/5 + 32

f2c :: Floating a => a -> a
f2c f = (f - 32) * 5/9
```

---

# Example-based tests with Hspec

```haskell
celsiusConversionSpec :: Spec
celsiusConversionSpec = 
  describe "Celsius conversions" $ do
    describe "c2k" $ do
      it "works for known examples" $ do
        c2k 0 `shouldBe` 273.15
        c2k 100 `shouldBe` 373.15
      it "fails for sub-abs-zero temperatures" $ do
        evaluate (c2k (-300)) `shouldThrow` anyException
    describe "c2f" $ do
      it "works for known examples" $ do
        c2f 0 `shouldBe` 32
        c2f 100 `shouldBe` 212
    describe "f2c" $ do
      it "works for known examples" $ do
        f2c 32 `shouldBe` 0
        f2c 212 `shouldBe` 100
```

---

# Example-based tests with Hspec

E.g., let's write a specification for `quadRoots`:

```haskell
quadRoots :: (Floating a, Ord a) => a -> a -> a -> (a, a)
quadRoots a b c 
    | disc >= 0 = ((-b + sqrt_d) / (2*a), 
                   (-b - sqrt_d) / (2*a))
    | otherwise = error "No real roots"
  where disc   = b^2 - 4*a*c
        sqrt_d = sqrt disc
```

---

# Example-based tests with Hspec

```haskell
quadRootsSpec :: Spec
quadRootsSpec = 
  describe "quadRoots" $ do
    it "works for known examples" $ do
      quadRoots 1 (-5) 6 `shouldBe` (3.0, 2.0)
      quadRoots 1 0 (-4) `shouldBe` (2.0, -2.0)
    it "fails when non-real roots exist" $ do
      evaluate (quadRoots 1 0 1) `shouldThrow` anyException
```

<!-- pause -->

Discussion: what are some problems / shortcomings of example-based testing?

---

# Property-based tests with QuickCheck

QuickCheck is the original property-based testing framework. To use it, we
specify properties for the unit being tested, and QuickCheck will automatically
generate test cases to check that the property holds.

<!-- pause -->

A property is function that takes test inputs and returns `Bool` or `Property`. 

- properties must be monomorphic (i.e., they can't have type variables), as
  QuickCheck needs concrete types to create random values

- "generators" produce random test cases, and can be customized

- if QuickCheck can falsify a property (i.e., prove that it doesn't hold), it
  tries to "shrink" the test cases to give us a minimal counterexample

---

# Property-based tests with QuickCheck

E.g., write a property to test that `c2f` and `f2c` are inverses:

```haskell
prop_c2f2c :: Double -> Bool
prop_c2f2c c = abs (f2c (c2f c) - c) < 0.0001
```

---

# Property-based tests with QuickCheck

E.g., write a property to test `mySum` using `sum` as a reference 
implementation:

```haskell
mySum :: (Eq a, Num a) => [a] -> a
mySum [] = 0
mySum (x:xs) = x + mySum xs

prop_sum :: [Integer] -> Bool
prop_sum xs = mySum xs == sum xs
```

What happens if you break `mySum`?

---

# Property-based tests with QuickCheck

E.g., try writing properties to test distributivity of multiplication over
addition and commutativity of addition:

```haskell
prop_distMultOverAdd :: Integer -> [Integer] -> Bool
prop_distMultOverAdd n xs = n * sum xs == sum (map (*n) xs)

prop_commAdd :: [Integer] -> Property
prop_commAdd xs = not (null xs) ==> 
                  sum xs == sum (reverse xs)
```

---

# Property-based tests with QuickCheck

E.g., write a property to test that `quadRoots` works correctly for perfect
squares and factorable quadratic equations:

```haskell
prop_perfSquare :: Double -> Bool
prop_perfSquare r = 
  let (r1, r2) = quadRoots 1 (-2*r) (r^2)
  in abs (r1 - r) < 0.0001 && abs (r2 - r) < 0.0001

prop_solvesFactored :: Double -> Double -> Bool
prop_solvesFactored r1 r2 = 
  let (r1', r2') = quadRoots 1 (-(r1+r2)) (r1*r2)
  in abs (r1' - r1) < 0.0001 && abs (r2' - r2) < 0.0001
```

---

# Property-based tests with QuickCheck

E.g., define a `Spec` combining property-based and unit tests:

```haskell
quadRootsSpec' :: Spec
quadRootsSpec' = 
  describe "quadRoots" $ do
    it "works for known examples" $ do
      quadRoots 1 (-5) 6 `shouldBe` (3.0, 2.0)
    it "fails when non-real roots exist" $ do
      evaluate (quadRoots 1 0 1) `shouldThrow` anyException
    it "works correctly with perfect squares" $ 
      property prop_perfSquare
    it "works correctly with factorable quadratic equations" $ 
      property prop_solvesFactored
```

---

# Test coverage

How much of our code are we actually testing? 

- are there functions we're never calling?

- are there patterns/guards/branches we're never matching/taking?

- are there unreachable sections of code?

<!-- pause -->

`stack test --coverage` generates a coverage report for all modules tested.

<!-- pause -->

100 percent test coverage is a noble goal!
