module Data.RankSelectArray.SDArraySpec where

import           Data.List.Unique
import           Data.RankSelectArray.Class
import           Data.RankSelectArray.ClassSpecGenerator
import           Data.RankSelectArray.SDArray
import qualified Data.Vector           as Vec
import           Test.Hspec
import           Test.QuickCheck


instance (RankSelectArray a) => Arbitrary (SDArray a) where
  arbitrary = do
    Sorted l <- arbitrary
    let positiveL = (uniq . dropWhile (<0)) l
    let onesCount = length positiveL
    let size = (last positiveL) + 1
    return $ fromOnes size onesCount positiveL

testBuildLower :: IO ()
testBuildLower = lowerArr `shouldBe` Vec.fromList [1, 3]
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


spec :: Spec
spec =
  describe "Tests for SDArray" $ do
    describe "Build" $ do
      it "Build lower bits" testBuildLower
      it "Build upper bits" testBuildUpper
      it "Build size" testBuildSize
    makeSpec (arbitrary :: Gen SDArray') "SDarray"
  


