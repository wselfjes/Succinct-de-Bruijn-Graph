module Data.RankSelectArray.VectorBitArraySpec where

import           Data.RankSelectArray.Class
import           Data.RankSelectArray.ClassSpecGenerator
import           Data.RankSelectArray.VectorBitArray
import           Test.Hspec
import           Test.QuickCheck
import           Data.Vector


instance Arbitrary VectorBitArray where
  arbitrary = do
    l <- arbitrary
    return $ VectorBitArray (fromList l)  

spec :: Spec
spec = makeSpec (arbitrary :: Gen VectorBitArray) "VectorBitArray"
