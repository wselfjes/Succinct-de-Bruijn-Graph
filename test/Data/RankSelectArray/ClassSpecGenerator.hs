{-# LANGUAGE ScopedTypeVariables #-}
module Data.RankSelectArray.ClassSpecGenerator where


import           Data.RankSelectArray.Class
import           Test.Hspec
import           Test.QuickCheck


selectProp :: RankSelectArray a => a -> Bool
selectProp v = go 1
  where
    go i
      | i > getOneCount v = True
      | otherwise = i == rank v True (select v True i) && go (i + 1)


makeSpec :: (RankSelectArray a, Arbitrary a, Show a) => Gen a -> String -> Spec
makeSpec (_::Gen a1) name = describe ("Tests for " ++ name) $ do
  it "Select && Rank combination" (property (selectProp :: a1 -> Bool))

