module Data.BitArrays.SDArraySpec where

import           Data.BitArrays.BitArray
import           Data.BitArrays.SDArray
import           Data.BitArrays.VectorBitArray
import qualified Data.Vector                   as Vec
import           Test.Hspec


testBuildLower :: IO ()
testBuildLower = lowerArr `shouldBe` Vec.fromList [1, 3]
  where
    bitArray = fromOnes [5, 7]
    lowerArr = lowerBits bitArray

testBuildUpper :: IO ()
testBuildUpper = upperArr `shouldBe` (generateEmpty 4 `setBits` [(1, True), (2, True)])
  where
    bitArray = fromOnes [5, 7]
    upperArr = upperBits bitArray

testBuildSize :: IO ()
testBuildSize = s `shouldBe` 7
  where
    bitArray = fromOnes [5, 7]
    s = bitVectorSize bitArray

testSelect :: IO ()
testSelect = s `shouldBe` 7
  where
    pos = 2
    bitArray = fromOnes [5, 7]
    s = sdarraySelect bitArray True pos

testRank :: IO ()
testRank = s `shouldBe` 0
  where
    bitArray = fromOnes [5, 7]
    s = sdarrayRank bitArray True 3

spec :: Spec
spec =
  describe "Tests for SDArray" $ do
    it "Build lower bits" testBuildLower
    it "Build upper bits" testBuildUpper
    it "Build size" testBuildSize
    it "Select" testSelect
    it "Rank" testRank

