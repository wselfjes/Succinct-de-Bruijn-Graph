module Data.RankSelectArray.DiffSpec where

import Test.Hspec
import Data.RankSelectArray.Diff
import Data.RankSelectArray.Class
import Data.RankSelectArray.SDArray


testRank :: IO ()
testRank = rankValue `shouldBe` 5
  where
    rankValue = rank diff True 8
    diff = Diff left right
    left = fromOnes 15 10 [1 .. 10] :: SDArray'
    right = fromOnes 15 3 [1, 3, 6] :: SDArray'

testSelect :: IO ()
testSelect = selectValue `shouldBe` 10
  where
    selectValue = select diff True 7
    diff = Diff left right
    left = fromOnes 15 10 [1 .. 10] :: SDArray'
    right = fromOnes 15 3 [1, 3, 6] :: SDArray'
    
spec :: Spec
spec = describe "Tests for difference between arrays" $ do
    it "Rank test for diff" testRank
    it "Select test for diff" testSelect
