module Data.BitArrays.VectorBitArraySpec where

import           Data.BitArrays.BitArray
import           Data.BitArrays.VectorBitArray
import           Test.Hspec

testSelect :: IO ()
testSelect = s `shouldBe` 1
  where
    bitArray = generateEmpty 4 `setBits` [(1, True), (2, True)] :: VectorBitArray
    s = select bitArray True 1

testRank :: IO ()
testRank = s `shouldBe` 1
  where
    bitArray = generateEmpty 10 `setBits` [(5, True)] :: VectorBitArray
    s = rank bitArray True 6

spec :: Spec
spec =
  describe "Tests for Vector Bit Array" $ do
    it "Select" testSelect
    it "Rank" testRank

