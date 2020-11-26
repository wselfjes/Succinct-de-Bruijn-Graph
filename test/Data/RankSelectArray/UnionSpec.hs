module Data.RankSelectArray.UnionSpec where

import Test.Hspec
import Data.RankSelectArray.Class
import Data.RankSelectArray.SDArray (SDArray')
import Data.RankSelectArray.Union


testSelect :: IO ()
testSelect = result `shouldBe` [1, 2, 3, 5, 7, 8, 9]
  where
    left = fromOnes 10 4 [1, 2, 3, 9] :: SDArray'
    right = fromOnes 10 3 [5,7,8] :: SDArray'
    union = Union left right
    result = map (select union True) [1 .. (getOneCount union)]


spec :: Spec
spec =
  describe "Tests for RankSelect Union" $
    it "Test select" testSelect
