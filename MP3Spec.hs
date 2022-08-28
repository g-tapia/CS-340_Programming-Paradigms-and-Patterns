{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MP3Spec (spec) where

import Test.Hspec
import Test.HUnit
import Test.HUnit.Approx
import Test.QuickCheck
import Test.QuickCheck.Gen
import Control.Exception
import MP3a
import MP3b

instance (Arbitrary a, Num a) => Arbitrary (BinTree a) where
  arbitrary = treeFromList . (++repeat 0) <$> arbitrary

data Hands = Hands [[Card]] deriving (Show, Eq)

instance Arbitrary Card where
  arbitrary = Card <$> chooseEnum (minBound, maxBound) <*> chooseEnum (minBound, maxBound)

instance Arbitrary Hands where
  arbitrary = Hands <$> listOf (take 5 <$> shuffle deck)

swapColors :: [Card] -> [Card]
swapColors = fmap $ \(Card r c) -> Card r (if c==maxBound then minBound else succ c)

incrementRank :: [Card] -> [Card]
incrementRank = fmap $ \(Card r c) -> Card (if r==maxBound then minBound else succ r) c

spec :: Spec
spec = do
  describe "Binary tree" $ do

    describe "treeRepeat" $ do
      it "returns the correct root node" $ do
        let (Node x _ _) = treeRepeat 10 in x `shouldBe` 10

    describe "treeToList, treeFromList" $ do
       it "treeFromLis . treeToList invariance" $ do
        property $ \(t :: BinTree Integer) -> (treeFromList . treeToList) t `shouldBe` t

    describe "treeNats" $ do
      it "treeNats' correctness" $ do
        property $ \n -> n >= 0 ==> treeToList treeNats !! (fromIntegral n) `shouldBe` succ n

    describe "treeVal" $ do
      it "treeVal correctness" $ do
        treeVal [L,L,L] treeNats `shouldBe` 8

    describe "treeFlip" $ do
       it "treeFlip . treeFlip invariance" $ do
         property $ \(t :: BinTree Integer) -> (treeFlip . treeFlip) t `shouldBe` t

    describe "treeIterate" $ do
       it "treeIterate correctness" $ do
         treeIterate succ 1 `shouldBe` treeNats


  describe "Poker stats" $ do

    describe "deck" $ do
      it "is the correct length" $ do
        length deck `shouldBe` 52

    describe "color invariance" $ do
      it "check that swapping colors doesn't effect the result " $ do
        property $ \(Hands hs) -> computeStats hs == computeStats (swapColors <$> hs)

    describe "rank invariance" $ do
      it "check that increasing rank of each card in a deck without Ace doesn't effect the result " $ do
        property $ \(Hands hs) -> let hs' = filter (not . elem Ace . fmap rank) hs in computeStats hs' == computeStats (incrementRank <$> hs')
