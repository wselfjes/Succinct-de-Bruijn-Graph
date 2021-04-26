{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.RankSelectArray.UnionSpec where

import           Data.List
import           Data.RankSelectArray.Class
import           Data.RankSelectArray.ClassSpecGenerator
import           Data.RankSelectArray.Diff
import           Data.RankSelectArray.SDArray            (SDArray')
import           Data.RankSelectArray.Union
import           Data.RankSelectArray.VectorBitArray
import           Test.Hspec
import           Test.QuickCheck                         hiding (getSize)


instance (RankSelectArray a, RankSelectArray b) => Arbitrary (Union a b) where
  arbitrary = do
    leftBools <- (arbitrary :: Gen [Bool])
    rightBools <- (arbitrary :: Gen [Bool])
    let rightBools' = filter (`notElem` leftBools) rightBools
    let left = elemIndices True leftBools
    let right = elemIndices True rightBools'
    Positive leftSizeOffset <- arbitrary
    Positive rightSizeOffset <- arbitrary
    let size = (max (length leftBools + leftSizeOffset) (length rightBools + rightSizeOffset))
    let leftArray = fromOnes size (length left) left
    let rightArray = fromOnes size (length right) right
    return $ Data.RankSelectArray.Union.Union leftArray rightArray


testSelect :: IO ()
testSelect = result `shouldBe` [1, 2, 3, 5, 7, 8, 9]
  where
    left = fromOnes 10 4 [1, 2, 3, 9] :: SDArray'
    right = fromOnes 10 3 [5,7,8] :: SDArray'
    union = Union left right
    result = map (select union True) [1 .. (getOneCount union)]

testRank :: IO ()
testRank = result `shouldBe` [1, 2, 3, 3, 4, 4, 5, 6, 7, 7]
  where
    left = fromOnes 10 4 [1, 2, 3, 9] :: SDArray'
    right = fromOnes 10 3 [5,7,8] :: SDArray'
    union = Union left right
    result = map (rank union True) [1 .. 10]

spec :: Spec
spec =
  describe "Tests for RankSelect Union" $ do
    describe "Simple test" $ do
      it "Test select" testSelect
      it "Test rank" testRank

    makeSpec (arbitrary :: Gen (Union VectorBitArray VectorBitArray)) "Unions with VectorBitArray"
