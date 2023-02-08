module Lect05Spec (spec) where

import Test.Hspec
import Test.HUnit
import Test.HUnit.Approx
import Test.QuickCheck
import Control.Exception
import Lect05 (c2k, c2f, f2c, mySum, quadRoots)
import Test.Hspec.QuickCheck (prop)


spec :: Spec
spec = describe "Lect05" $ do
  describe "Celsius conversions" $ do
    describe "c2k" $ do
      it "works for known examples" $ do
        c2k 0 `shouldBe` 273.15
        c2k 100 `shouldBe` 373.15
      it "fails for sub-abs-zero temperatures" $ do
        evaluate (c2k (-274)) `shouldThrow` anyException
    describe "c2f" $ do
      it "works for known examples" $ do
        c2f 0 `shouldBe` 32
        c2f 100 `shouldBe` 212
        c2f 500.1 `shouldSatisfy` (=~= 932.18)
    describe "f2c" $ do
      it "works for known examples" $ do
        f2c 32 `shouldBe` 0
        f2c 212 `shouldBe` 100
        f2c 932.18 `shouldSatisfy` (=~= 500.1)

  describe "mySum" $ do
    it "matches the reference implementation" $ 
      property prop_sum
    it "demonstrates distributivity w.r.t. multiplication" $
      property prop_distMultOverAdd
    it "demonstrates commutativity" $
      property prop_commAdd

  describe "quadRoots" $ do
    it "works for known examples" $ do
      quadRoots 1 3 2 `shouldMatchTuple` (-1, -2)
      quadRoots 1 4 4 `shouldMatchTuple` (-2, -2)
      quadRoots 1 5 6 `shouldMatchTuple` (-2, -3)
    it "fails when non-real roots exist" $ do
      evaluate (quadRoots 1 0 1) `shouldThrow` anyException
    it "works correctly with perfect squares" $ 
      property prop_perfSquare
    it "works correctly with factorable quadratic equations" $ 
      property prop_solvesFactored
        

infix 4 =~=
(=~=) :: (Floating a, Ord a) =>  a -> a -> Bool
x =~= y = abs (x - y) < 0.0001

shouldMatchTuple :: (Eq a, Show a) => (a, a) -> (a, a) -> Expectation
shouldMatchTuple (x1, x2) (y1, y2) = [x1, x2] `shouldMatchList` [y1, y2]


prop_c2f2c :: Double -> Bool
prop_c2f2c c = f2c (c2f c) =~= c

cTemp :: Gen Double
cTemp = choose (-273.15, 1000)

prop_c2f2c' :: Property
prop_c2f2c' = forAll cTemp prop_c2f2c

prop_c2f2c'' :: Double -> Property
prop_c2f2c'' c = c >= -273.15 ==> f2c (c2f c) =~= c


prop_sum :: [Integer] -> Bool
prop_sum xs = mySum xs == sum xs


prop_distMultOverAdd :: Integer -> [Integer] -> Bool
prop_distMultOverAdd n xs = mySum [n*x | x <- xs] == n * mySum xs


prop_commAdd :: [Integer] -> Property
prop_commAdd xs = forAll (shuffle xs) (\ys -> mySum xs == mySum ys)


prop_perfSquare :: Double -> Bool
prop_perfSquare f = r1 =~= r2
  where b = 2*f
        c = f^2
        (r1,r2) = quadRoots 1 b c


prop_solvesFactored :: Double -> Double -> Bool
prop_solvesFactored f1 f2 = r1^2 + b*r1 + c =~= 0 
                         && r2^2 + b*r2 + c =~= 0
  where b = f1 + f2
        c = f1 * f2
        (r1,r2) = quadRoots 1 b c
