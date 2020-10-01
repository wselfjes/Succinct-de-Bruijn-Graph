{-# LANGUAGE ScopedTypeVariables #-}
module Data.RankSelectArray.ClassSpecGenerator where


import           Data.RankSelectArray.Class
import           Test.Hspec
import           Test.QuickCheck
import           Test.Hspec.Core.QuickCheck (modifyMaxSuccess)


safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast l = (Just . last) l


selectRankProp :: RankSelectArray a => a -> Bool
selectRankProp arr = all (\i -> i == rank arr True (select arr True i)) [1 .. getOneCount arr]

makeSpec :: (RankSelectArray a, Arbitrary a, Show a) => Gen a -> String -> Spec
makeSpec (_::Gen a1) name = describe ("Tests for " ++ name) $ do
  modifyMaxSuccess (const 10000) $ it "Select && Rank combination" (property (selectRankProp :: a1 -> Bool))

