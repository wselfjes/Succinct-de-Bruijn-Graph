module Data.RankSelectArray.SDArraySpec where

import           Data.RankSelectArray.Class
import           Data.RankSelectArray.SDArray
import qualified Data.Vector                    as Vec
import qualified HaskellWorks.Data.PackedVector as PV
import           Test.Hspec


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

testSelect :: IO ()
testSelect = s `shouldBe` 7
  where
    pos = 2
    bitArray = fromOnes 8 2 [5, 7] :: SDArray'
    s = sdarraySelect bitArray True pos

testRank :: IO ()
testRank = s `shouldBe` 0
  where
    bitArray = fromOnes 8 2 [5, 7] :: SDArray'
    s = sdarrayRank bitArray True 3

testGetBit :: IO ()
testGetBit = bit `shouldBe` False
  where
    bitArray = fromOnes 8 2 [5, 7] :: SDArray'
    bit = sdarrayGetBit 6 bitArray

spec :: Spec
spec =
  describe "Tests for SDArray" $ do
    it "Build lower bits" testBuildLower
    it "Build upper bits" testBuildUpper
    it "Build size" testBuildSize
    it "Select" testSelect
    it "Rank" testRank
    it "Get bit" testGetBit

