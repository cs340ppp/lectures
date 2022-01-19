module Lect05Spec (spec) where

import Test.Hspec
import Test.HUnit
import Test.HUnit.Approx
import Test.QuickCheck
import Control.Exception
import Lect05 (nand, c2h, distance, quadRoots)


spec :: Spec
spec = describe "Lect04" $ do
  describe "nand" $ do
    it "works correctly" $ do
      assertEqual "nand True True"   False (nand True True)
      assertEqual "nand True False"  True  (nand True False)
      assertEqual "nand False True"  True  (nand False True)
      assertEqual "nand False False" True  (nand False False)

  describe "c2h" $ do
    it "works for too-hot temperatures" $ do
      c2h 38 `shouldBe` "too hot"
      c2h 50 `shouldBe` "too hot"
    it "works for too-cold temperatures" $ do
      c2h (-18) `shouldBe` "too cold"
      c2h (-30) `shouldBe` "too cold"
    it "works for tolerable temperatures" $ do
      c2h 37 `shouldBe` "tolerable"
      c2h (-17) `shouldBe` "tolerable"
      c2h 25 `shouldBe` "tolerable"
    
  describe "distance" $ do
    it "always returns a positive value" $ 
      property prop_alwaysPositive
    it "is commutative" $ 
      property prop_commDistance
    it "matches known results" $ do
      distance (0,0) (3,4) `shouldBe` 5
      distance (10,10) (13,14) `shouldBe` 5
      
  describe "quadRoots" $ do
    it "works correctly with perfect squares" $ 
      property prop_perfSquare
    it "works correctly with factorable quadratic equations" $ 
      property prop_solvesFactored
    it "fails on equations with negative discriminants" $ do
      evaluate (quadRoots 1 1 1) `shouldThrow` anyErrorCall
        

infix 4 =~=
(=~=) :: (Floating a, Ord a) =>  a -> a -> Bool
x =~= y = abs (x - y) < 0.0001


prop_alwaysPositive :: (Double,Double) -> (Double,Double) -> Bool
prop_alwaysPositive (x1,y1) (x2,y2) = distance (x1,y1) (x2,y2) >= 0


prop_commDistance :: (Double,Double) -> (Double,Double) -> Bool
prop_commDistance (x1,y1) (x2,y2) = d1 =~= d2
  where d1 = distance (x1,y1) (x2,y2)
        d2 = distance (x2,y2) (x1,y1)


prop_zeroDistance :: (Double,Double) -> Bool
prop_zeroDistance (x,y) = distance (x,y) (x,y) =~= 0


prop_straightLineDistances :: (Double,Double) -> Double -> Bool
prop_straightLineDistances (x,y) d = vert_d =~= abs d && hori_d =~= abs d
  where vert_d = distance (x,y) (x,y+d)
        hori_d = distance (x,y) (x+d,y)


prop_perfSquare :: Double -> Bool
prop_perfSquare f = r1 =~= r2
  where b = 2*f
        c = f^2
        (r1,r2) = quadRoots 1 b c


prop_solvesFactored :: Double -> Double -> Bool
prop_solvesFactored f1 f2 = r1^2 + b*r1 + c =~= 0 && r2^2 + b*r2 + c =~= 0
  where b = f1 + f2
        c = f1 * f2
        (r1,r2) = quadRoots 1 b c
