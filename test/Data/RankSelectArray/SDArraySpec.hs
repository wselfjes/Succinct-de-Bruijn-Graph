module Data.RankSelectArray.SDArraySpec where

import           Data.List.Unique
import           Data.List
import           Data.RankSelectArray.Class
import           Data.RankSelectArray.ClassSpecGenerator
import           Data.RankSelectArray.SDArray
import           Data.RankSelectArray.VectorBitArray
import           Data.RankSelectArray.DenseArray
import qualified Data.Vector                    as Vec
import qualified HaskellWorks.Data.PackedVector as PV
import           Test.Hspec
import           Test.QuickCheck
import           Data.Maybe
import           Test.Hspec.Core.QuickCheck              (modifyMaxSuccess)


instance (RankSelectArray a) => Arbitrary (SDArray a) where
  arbitrary = do
    bools <- (arbitrary :: Gen [Bool])
    let l = elemIndices True bools
    let onesCount = length l
    Positive offset <- arbitrary
    let size = (fromMaybe 0 (safeLast l)) + offset
    return $ fromOnes size onesCount l

testBuildLower :: IO ()
testBuildLower = lowerArr `shouldBe` PV.fromList 2 [1, 3]
  where
    bitArray = fromOnes 8 2 [5, 7] :: SDArray'
    lowerArr = lowerBits bitArray

testBuildUpper :: IO ()
testBuildUpper = upperArr `shouldBe` (generateEmpty 4 `setBits` [(1, True), (2, True)])
  where
    bitArray = fromOnes 8 2 [5, 7] :: SDArray'
    upperArr = upperBits bitArray

testBuildSize :: IO ()
testBuildSize = s `shouldBe` 8
  where
    bitArray = fromOnes 8 2 [5, 7] :: SDArray'
    s = bitVectorSize bitArray

compareSDArrayWithBackends :: NonEmptyList Bool -> Bool
compareSDArrayWithBackends (NonEmpty bools) = all testSelect [1 .. onesCount] && all testRank [0 .. size - 1]
  where
    l = elemIndices True bools
    onesCount = length l
    size = length bools
    sdarrayVectorBitArray = fromOnes onesCount size l :: SDArray VectorBitArray
    sdarrayDenseArray = fromOnes onesCount size l :: SDArray (DenseArray VectorBitArray)
    testSelect i = selectTrueTest
      where
        selectTrueTest = select sdarrayVectorBitArray True i == select sdarrayDenseArray True i
    testRank i = rankTrueTest 
      where
        rankTrueTest = rank sdarrayVectorBitArray True i == rank sdarrayDenseArray True i

testGetBit :: IO ()
testGetBit = bit `shouldBe` False
  where
    bitArray = fromOnes 8 2 [5, 7] :: SDArray'
    bit = sdarrayGetBit 6 bitArray

spec :: Spec
spec =
  describe "Tests for SDArray" $ do
    describe "Build" $ do
      it "Build lower bits" testBuildLower
      it "Build upper bits" testBuildUpper
      it "Build size" testBuildSize
      modifyMaxSuccess (const 10000) $ it "Compare vector bit array and dense array as backends for sdarray" (property compareSDArrayWithBackends)

    makeSpec (arbitrary :: Gen SDArray') "SDarray with VectorBitArray"
    makeSpec (arbitrary :: Gen (SDArray (DenseArray VectorBitArray))) "SDarray with DenseArray"
