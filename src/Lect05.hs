% CS 340: Programming Paradigms and Patterns
% Lect 05 - Testing
% Michael Lee

\begin{code}
module Lect05 where
import Control.Exception
import Test.Hspec
import Test.QuickCheck
\end{code}

Testing
=======

Agenda:
  - What is testing?
  - Approaches to testing & verification
  - Hspec testing framework
  - Example-based tests with Hspec
  - Property-based tests with QuickCheck
  - Test coverage


What is testing?
----------------

A *test* verifies that some aspect of a system works to specification.

Testing tools can help:

  - simplify test specification, discovery, execution, and reporting

  - ensure that code changes don't break existing functionality (no regressions)

  - determine code coverage (how much of the codebase is actually run)

  - eliminate code "lint" (aka "dead code")


Approaches to testing & verification
------------------------------------

General strategy: Test-Driven Development (TDD)

  - Write tests *first*, ensure they fail, then write code to get them to pass

  - After all tests pass, any future code refactoring requires re-running tests


But how to write tests? (How to verify correctness?)

  - *Static tests* are carried out by a compiler, which checks for syntax and
     type related errors. We write *type signatures* to help the compiler.

  - *Unit tests* check that "units" of code (e.g., functions, classes) work as 
    expected. Their specification/execution is facilitated by test frameworks.

      - *Example-based tests* explicitly declare the expected results (e.g.,
        return value, output, exception) for different inputs and/or state.
      
      - *Property-based tests* declare high-level "properties" (aka invariants) 
        that must hold true for all inputs and/or state. Specific cases are 
        automatically generated and checked.
              
  - *Formal verification* may be done at a higher level of abstraction. It is
    typically done by a theorem prover, which checks for logical errors by
    proving that the program satisfies a set of logical properties.


Hspec testing framework
-----------------------

Hspec gives us a way to specify tests in a human-legible way:

\begin{code}
someSpec :: Spec
someSpec = 
  describe "someFunc" $ do
    it "fulfills some expectation ..." $
      pendingWith "Need to flesh out this test"
    it "fulfills some other expectation ..." $
      pending
\end{code}

  - Run a `Spec` using `hspec`.

  - Hspec supports both unit tests and property-based tests

  - `stack test` will run "test/Spec.hs", which will automatically discover
    all "*Spec.hs" files in the "test" directory and run their "spec" functions

    - E.g., all tests for this lecture are in "test/Lect05Spec.hs"


Example-based tests with Hspec
------------------------------

Hspec provides various functions for creating `Expectations`:

  - `shouldBe` / `shouldNotBe`
  - `shouldSatisfy` / `shouldNotSatisfy`
  - `shouldMatchList` / `shouldNotMatchList`
  - `shouldThrow` / `shouldNotThrow`

E.g., let's write a specification for `c2k`, `c2f`, `f2c`:

\begin{code}
c2k :: (Ord a, Floating a) => a -> a
c2k c | c >= 0 = c + 273.15
      | otherwise = error "Temperature below absolute zero"


c2f :: Floating a => a -> a
c2f c = c * 9/5 + 32


f2c :: Floating a => a -> a
f2c f = (f - 32) * 5/9


celsiusConversionSpec :: Spec
celsiusConversionSpec = 
  describe "Celsius conversions" $ do
    describe "c2k" $ do
      it "works for known examples" $ do
        pending
      it "fails for sub-abs-zero temperatures" $ do
        pending
    describe "c2f" $ do
      it "works for known examples" $ do
        pending
    describe "f2c" $ do
      it "works for known examples" $ do
        pending
\end{code}


E.g., let's write a specification for `quadRoots`

\begin{code}
quadRoots :: (Floating a, Ord a) => a -> a -> a -> (a, a)
quadRoots a b c 
    | disc >= 0 = ((-b + sqrt_d) / (2*a), (-b - sqrt_d) / (2*a))
    | otherwise = error "No real roots"
  where disc   = b^2 - 4*a*c
        sqrt_d = sqrt disc


quadRootsSpec :: Spec
quadRootsSpec = 
  describe "quadRoots" $ do
    it "works for known examples" $ do
      pending
    it "fails when non-real roots exist" $ do
      pending
\end{code}


Discussion: what are some problems / shortcomings of example-based testing?


Property-based tests with QuickCheck
------------------------------------

QuickCheck is the original property-based testing framework. To use it, we
specify properties for the unit being tested, and QuickCheck will automatically
generate test cases to check that the property holds.

A property is function that takes test inputs and returns `Bool` or `Property`. 

  - properties must be monomorphic (i.e., they can't have type variables), as
    QuickCheck needs concrete types to create random values

  - "generators" produce random test cases, and can be customized

  - if QuickCheck can falsify a property (i.e., prove that it doesn't hold), it
    tries to "shrink" the test cases to give us a minimal counterexample


E.g., write a property to test that `c2f` and `f2c` are inverses:


\begin{code}
prop_c2f2c :: Double -> Bool
prop_c2f2c = undefined
\end{code}


E.g., write a property to test `mySum` using `sum` as a reference implementation (what happens if you break `mySum`?):

\begin{code}
mySum :: (Eq a, Num a) => [a] -> a
mySum [] = 0
mySum (x:xs) = x + mySum xs


prop_sum :: [Integer] -> Bool
prop_sum = undefined
\end{code}


E.g., try writing properties to test distributivity of multiplication over
addition and commutativity of addition:

\begin{code}
prop_distMultOverAdd :: Integer -> [Integer] -> Bool
prop_distMultOverAdd = undefined


prop_commAdd :: [Integer] -> Property
prop_commAdd = undefined
\end{code}


E.g., write a property to test that `quadRoots` works correctly for perfect
squares and factorable quadratic equations:

\begin{code}
prop_perfSquare :: Double -> Bool
prop_perfSquare = undefined


prop_solvesFactored :: Double -> Double -> Bool
prop_solvesFactored = undefined
\end{code}


E.g., define a `Spec` combining property-based and unit tests:

\begin{code}
quadRootsSpec' :: Spec
quadRootsSpec' = 
  describe "quadRoots" $ do
    it "works for known examples" $ do
      pending
    it "fails when non-real roots exist" $ do
      pending
    it "works correctly with perfect squares" $ 
      pending
    it "works correctly with factorable quadratic equations" $ 
      pending
\end{code}


Test coverage
-------------

How much of our code are we actually testing? 

  - are there functions we're never calling?
  
  - are there patterns/guards/branches we're never matching/taking?

  - are there unreachable sections of code?

`stack test --coverage` generates a coverage report for all modules tested.

100 percent test coverage is a noble goal!
