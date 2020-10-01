module Data.RankSelectArray.DenseArraySpec where

import           Data.List
import           Data.List.Unique
import           Data.Maybe
import           Data.RankSelectArray.Class
import           Data.RankSelectArray.ClassSpecGenerator
import           Data.RankSelectArray.DenseArray
import           Data.RankSelectArray.VectorBitArray
import           Test.Hspec
import           Test.Hspec.Core.QuickCheck              (modifyMaxSuccess)
import           Test.QuickCheck hiding (getSize)

instance (RankSelectArray a) => Arbitrary (DenseArray a) where
  arbitrary = do
    bools <- (arbitrary :: Gen [Bool])
    let l = elemIndices True bools
    let onesCount = length l
    let size = length bools
    return $ fromOnes size onesCount l


testSelect :: IO ()
testSelect = selectedValue `shouldBe` 1
  where
    selectedValue = select arr True 2
    arr = fromOnes 4 2 [0, 1] :: DenseArray VectorBitArray

testRank :: IO ()
testRank = rankedValue `shouldBe` 2
  where
    rankedValue = rank arr True 1
    arr = fromOnes 4 2 [0, 1] :: DenseArray VectorBitArray

inverseRankSelectProp :: DenseArray VectorBitArray -> Bool
inverseRankSelectProp arr = all (\i -> i == rank arr False (select arr False i)) [1 .. (size arr - getOneCount arr)]

checkSelectWithVectorBitArrayProp :: DenseArray VectorBitArray -> Bool
checkSelectWithVectorBitArrayProp arr = all (\i -> select (storage arr) True i == select arr True i) [1 .. getOneCount arr]

checkRankWithVectorBitArrayProp :: DenseArray VectorBitArray -> Bool
checkRankWithVectorBitArrayProp arr = all (\i -> rank (storage arr) True i == rank arr True i) [1 .. getOneCount arr]

inverseCheckSelectWithVectorBitArrayProp :: DenseArray VectorBitArray -> Bool
inverseCheckSelectWithVectorBitArrayProp arr = all (\i -> select (storage arr) False i == select arr False i) [1 .. (getSize arr - getOneCount arr)]

inverseCheckRankWithVectorBitArrayProp :: DenseArray VectorBitArray -> Bool
inverseCheckRankWithVectorBitArrayProp arr = all (\i -> rank (storage arr) False i == rank arr False i) [1 .. (getSize arr - getOneCount arr)]


spec :: Spec
spec =
  describe "Tests for DenseArray" $ do
    describe "Simple test" $ do
        it "Select" testSelect
        it "Rank" testRank
    describe "QuickCheck tests" $ do
        modifyMaxSuccess (const 10000) $ it "Inverse rank select combination" (property inverseRankSelectProp)
        modifyMaxSuccess (const 10000) $ it "Compare select with VectorBitArray" (property checkSelectWithVectorBitArrayProp)
        modifyMaxSuccess (const 10000) $ it "Compare rank with VectorBitArray" (property checkRankWithVectorBitArrayProp)
        modifyMaxSuccess (const 10000) $ it "Inverse compare select with VectorBitArray" (property inverseCheckSelectWithVectorBitArrayProp)
        modifyMaxSuccess (const 10000) $ it "Inverse compare rank with VectorBitArray" (property inverseCheckRankWithVectorBitArrayProp)
    makeSpec (arbitrary :: Gen (DenseArray VectorBitArray)) "DenseArray with VectorBitArray"
