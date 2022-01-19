% CS 340: Programming Paradigms and Patterns
% Lect 05 - Testing
% Michael Lee

> module Lect05 where
> import Data.Char
> import Control.Exception
> import Test.HUnit
> import Test.HUnit.Approx
> import Test.Hspec
> import Test.QuickCheck

Testing
=======

Agenda:
  - What is testing and why do we do it?
  - Approaches to testing
  - Hspec testing framework
  - Unit tests with Hspec/HUnit
  - Property-based testing with QuickCheck
  - Test coverage


What is testing and why do we do it?
------------------------------------

A *test* determines if some aspect of a system works according to a given
specification. We test our code to help ensure correctness!

Testing tools can also help us:

  - locate and fix bugs

  - verify logical invariants

  - ensure that APIs are consistent

  - determine code coverage

  - eliminate code "lint"


Approaches to testing
---------------------

Most modern software development models emphasize early and continuous testing
to ensure software quality. 

Test-Driven Development (TDD) asks developers to write tests *first*, ensure
that they fail (why?), and only then write code to get the tests to pass. After
all tests pass, any future code refactoring --- i.e., restructuring to improve
modularity, speed, efficiency, legibility, etc. --- require re-running tests.

Tests can be specified and designed in many different ways:

  - Haskell's type system supports static testing and verification. The
    compiler uses type inference and explicit type declarations to ensure no
    type-related errors will occur at runtime. (But the type system doesn't
    guard against logical errors!)

  - *Unit tests* verify that parts of a program (e.g., functions or classes)
    work as expected, by manually specifying the desired results (e.g., return
    value or output) for different combinations of input and/or state.

  - *Property-based tests* verify that parts of a program consistently maintain
    specified high-level properties, which are tested via multiple automatically
    generated inputs.

  - Test frameworks simplify and automate the specification, discovery, and
    execution of tests. They can also help determine code coverage --- i.e.,
    how much of the codebase is actually being tested.


Hspec testing framework
-----------------------

Hspec gives us a way to specify tests in a human-legible way:

> someSpec :: Spec
> someSpec = 
>   describe "someFunc" $ do
>     it "fulfills some expectation ..." $
>       pendingWith "Need to flesh out this test"
>     it "fulfills some other expectation ..." $
>       pending


  - You can run a `Spec` using `hspec`.

  - Hspec supports both unit tests and property-based tests

  - `stack test` will automatically run "test/Spec.hs", which will discover
    all "*Spec.hs" files in the "test" directory and run their "spec" functions.

  - You can test a specific module with: `stack test --test-arguments "-m MP.MP1"`

  - We typically put all test code in the "test" directory --- all `Spec`s
    defined in this file are in "Lect05Spec.hs"


Unit tests with Hspec/HUnit
---------------------------

HUnit is a unit testing library that gives us the `Assertion` type, which can be
executed to determine if associated tests pass or fail. Assertions include
`assertBool`, `assertEqual`, `assertApproxEqual`. E.g.,

> tests1 = TestList [
>            TestCase $ assertBool "even 8" (even 8),
>            TestCase $ assertEqual "2**10" 1024 (2**10),
>            TestCase $ assertApproxEqual "sqrt 2" 0.001 1.414 (sqrt 2)
>          ]
>
> tests2 = TestList [ -- broken tests!
>            TestCase $ assertBool "even 7" (even 7),
>            TestCase $ assertEqual "2**12" 1024 (2**12),
>            TestCase $ assertApproxEqual "sqrt 2" 0.0001 1.414 (sqrt 2)
>          ]

We can run HUnit tests with `runTestTT`.

---

Hspec automatically creates test cases for us when we use the "describe"/"it"
syntax to create `Spec`s. E.g., to test `nand`:

> nand :: Bool -> Bool -> Bool
> nand True True = False
> nand _    _    = True
>
>
> nandSpec :: Spec
> nandSpec = 
>   describe "nand" $ do
>     it "works correctly" $ do
>       assertEqual "nand True True"   False (nand True True)
>       assertEqual "nand True False"  True  (nand True False)
>       assertEqual "nand False True"  True  (nand False True)
>       assertEqual "nand False False" True  (nand False False)

---

We can also use Hspec functions `shouldBe`, `shouldSatisfy`, and `shouldThrow`
instead of HUnit assertions to describe test expectations. E.g., 

    (2**10) `shouldBe` 1024

    8 `shouldSatisfy` even

    someAction `shouldThrow` anyException


E.g., to test `c2h` (note that we try to exhaustively test edge cases):

> c2f :: Fractional a => a -> a
> c2f c = c * 9/5 + 32
>
> c2h :: (Floating a, Ord a) => a -> String
> c2h c | f < 0     = "too cold"
>       | f > 100   = "too hot"
>       | otherwise = "tolerable"
>   where f = c2f c
>
>
> c2hSpec :: Spec
> c2hSpec = 
>   describe "c2h" $ do
>     it "works for too-hot temperatures" $ do
>       c2h 38 `shouldBe` "too hot"
>       c2h 50 `shouldBe` "too hot"
>     it "works for too-cold temperatures" $ do
>       c2h (-18) `shouldBe` "too cold"
>       c2h (-30) `shouldBe` "too cold"
>     it "works for tolerable temperatures" $ do
>       c2h 37 `shouldBe` "tolerable"
>       c2h (-17) `shouldBe` "tolerable"
>       c2h 25 `shouldBe` "tolerable"


Property-based testing with QuickCheck
--------------------------------------

QuickCheck lets us define a `Property` from a predicate (a function that
evaluates to a Boolean). The predicate is called automatically for many randomly
selected inputs (based on the input types) to test that the property holds.

  - properties must be monomorphic (i.e., they can't have type variables), as
    QuickCheck needs concrete types to create random values

  - the generation of random test cases can be customized to reflect likely or
    relevant inputs

  - if QuickCheck can falsify a property (i.e., prove that it doesn't hold for
    an input), it attempts to "shrink" the test cases to give us a minimal 
    counterexample

  - takes the chore out of writing individual test cases, and our property
    specifications serve as useful high-level documentation!


E.g., some arithmetic properties:

> prop_commAdd :: Integer -> Integer -> Bool
> prop_commAdd x y = x + y == y + x
>
>
> prop_distMultOverAdd :: Integer -> Integer -> Integer -> Bool
> prop_distMultOverAdd x y z = x * (y + z) == x * y + x * z

Test predicates by passing them to `quickCheck` or `verboseCheck`.

---

E.g., shrinking in action:

> prop_wrongSum :: [Int] -> Bool
> prop_wrongSum xs = sum xs == brokenSum xs
>   where brokenSum [] = 0
>         brokenSum (8:xs) = 7 + brokenSum xs
>         brokenSum (x:xs) = x + brokenSum xs

---

We can also use functions like `collect` and `classify` to help log test
information. 

> prop_commAdd' :: Integer -> Integer -> Property
> prop_commAdd' x y = collect (x+y) $ x + y == y + x
>
>
> prop_distMultOverAdd' :: Integer -> Integer -> Integer -> Property
> prop_distMultOverAdd' x y z = 
>   classify (x == 0) "zero" $ x * (y + z) == x * y + x * z

---

E.g., define properties for Euclidean distance function `distance`:

> distance :: (Floating a, Eq a) => (a,a) -> (a,a) -> a
> distance (x1,y1) (x2,y2) = sqrt (dx^2 + dy^2)
>   where dx = x1-x2
>         dy = y1-y2
>
>
> prop_alwaysPositive :: (Double,Double) -> (Double,Double) -> Bool
> prop_alwaysPositive (x1,y1) (x2,y2) = distance (x1,y1) (x2,y2) >= 0
>
>
> -- define an operator for approximate equality
> infix 4 =~=
> (=~=) :: (Floating a, Ord a) =>  a -> a -> Bool
> x =~= y = abs (x - y) < 0.0001
>
>
> prop_commDistance :: (Double,Double) -> (Double,Double) -> Bool
> prop_commDistance (x1,y1) (x2,y2) = d1 =~= d2
>   where d1 = distance (x1,y1) (x2,y2)
>         d2 = distance (x2,y2) (x1,y1)
>
> 
> prop_zeroDistance :: (Double,Double) -> Bool
> prop_zeroDistance (x,y) = distance (x,y) (x,y) =~= 0
>
>
> prop_straightLineDistances :: (Double,Double) -> Double -> Bool
> prop_straightLineDistances (x,y) d = vert_d =~= abs d && hori_d =~= abs d
>   where vert_d = distance (x,y) (x,y+d)
>         hori_d = distance (x,y) (x+d,y)


Define a `Spec` combining property-based and unit tests:

> distanceSpec :: Spec
> distanceSpec = 
>   describe "distance" $ do
>     it "always returns a positive value" $ 
>       property prop_alwaysPositive
>     it "is commutative" $ 
>       property prop_commDistance
>     it "straight line distances are computed correctly" $
>       property prop_straightLineDistances
>     it "matches known results" $ do
>       distance (0,0) (3,4) `shouldSatisfy` (=~= 5)
>       distance (10,10) (13,14) `shouldSatisfy` (=~= 5)

---

E.g., define properties for quadratic roots function `quadRoots`:

> quadRoots :: (Floating a, Ord a) => a -> a -> a -> (a, a)
> quadRoots a b c 
>     | disc >= 0 = ((-b + sqrt_d) / (2*a), (-b - sqrt_d) / (2*a))
>     | otherwise = error "No real roots"
>   where disc   = b^2 - 4*a*c
>         sqrt_d = sqrt disc
>
>
> prop_perfSquare :: Double -> Bool
> prop_perfSquare f = r1 =~= r2
>   where b = 2*f
>         c = f^2
>         (r1,r2) = quadRoots 1 b c
>
>
> prop_solvesFactored :: Double -> Double -> Bool
> prop_solvesFactored f1 f2 = r1^2 + b*r1 + c =~= 0 && r2^2 + b*r2 + c =~= 0
>   where b = f1 + f2
>         c = f1 * f2
>         (r1,r2) = quadRoots 1 b c


Define a `Spec` combining property-based and unit tests:

> quadRootsSpec :: Spec
> quadRootsSpec = 
>   describe "quadRoots" $ do
>     it "works correctly with perfect squares" $ 
>       property prop_perfSquare
>     it "works correctly with factorable quadratic equations" $ 
>       property prop_solvesFactored
>     it "fails on equations with negative discriminants" $ do
>       evaluate (quadRoots 1 1 1) `shouldThrow` anyErrorCall


Test coverage
-------------

How much of our code are we actually testing? 

  - are there functions we're never calling?

  - are there patterns we're never matching?

  - are there guards/branches we aren't taking?

  - are there unneeded local bindings? 

  - are there unreachable sections of code?

`stack test --coverage` generates a coverage report for all modules tested.

100% test coverage is a noble and worthwhile goal!
