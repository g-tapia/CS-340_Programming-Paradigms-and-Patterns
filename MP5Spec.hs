{-# LANGUAGE ImplicitParams #-}

module MP5Spec (spec) where

import Test.Hspec
import Test.HUnit
import Test.HUnit.Approx
import Test.QuickCheck
import Control.Exception
import MP5a
import MP5b

spec :: Spec
spec = do
  describe "Search" $ do
    it "Works as expected" $ do
      knightsTour (5, 5) (2, 2) `shouldNotBe` Nothing
  describe "Minimax" $ do
    it "Works as expected" $ do
      result <- playAI2 1
      result `shouldBe` Just X
      result <- playAI2 2
      result `shouldBe` Just X
      result <- playAI2 3
      result `shouldBe` Just O
