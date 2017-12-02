module C_RecursionSpec where

import           C_Recursion
import           Test.Hspec
import           Test.Hspec.Checkers
import           Test.QuickCheck
import           Test.QuickCheck.Classes

addableAssocProp x y z = (x `madd` (y `madd` z)) == ((x `madd` y) `madd` z)
addableLeftIdProp x = x == (x `madd` mzero)
addableRightIdProp x = x == (mzero `madd` x)

spec = do
  describe "sumList" $ do
   it "sums the elements of a list" $ do
     sumList [1, 2, 3] `shouldBe` 6
     sumList [1, 2, 3, 4] `shouldBe` 10
     sumList [5, 10, 15] `shouldBe` 30

  describe "sumAll" $ do
    it "should be able to sum reducables of addables" $ do
      sumAll [1, 2, 3] `shouldBe` (6 :: Integer)
      sumAll [1, 2, 3, 4] `shouldBe` (10 :: Integer)
      sumAll [5, 10, 15] `shouldBe` (30 :: Integer)

      sumAll [Just 2, Just 3] `shouldBe` Just (5 :: Integer)

      sumAll ["Hello", " ", "World", "!"] `shouldBe` "Hello World!"

  -- property based testing
  describe "Addable Integer" $ do
    it "madd should be associative" $ property $
      \x y z -> addableAssocProp x y (z :: Integer)
    it "mzero and madd should hold left identity" $ property $
      \x -> addableLeftIdProp (x :: Integer)
    it "mzero and madd should hold right identity" $ property $
      \x -> addableRightIdProp (x :: Integer)

  describe "Addable Maybe Integer" $ do
    it "madd should be associative" $ property $
      \x y z -> addableAssocProp x y (z :: Maybe Integer)
    it "mzero and madd should hold left identity" $ property $
      \x -> addableLeftIdProp (x :: Maybe Integer)
    it "mzero and madd should hold right identity" $ property $
      \x -> addableRightIdProp (x :: Maybe Integer)

  -- common typeclasses have law checkers
  describe "[Int]" $ do
    testBatch  (monoid (undefined :: [Int]))
    testBatch (traversable (undefined :: [] (Int, Double, String)))
