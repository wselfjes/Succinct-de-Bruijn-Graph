{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Types.DNASpec where

import           Test.Hspec
import           Test.Hspec.Core.QuickCheck
import           Test.QuickCheck
import           Types.DNA

instance Arbitrary Nucleotide where
  arbitrary = elements allNucleotides

instance Arbitrary DNASequence where
  arbitrary = DNASequence <$> arbitrary
  shrink = genericShrink

testOverlappingDNASequence :: IO ()
testOverlappingDNASequence =(("AAAC" :: DNASequence) `mergeDNASequence` ("ACGT" :: DNASequence))
                             `shouldBe` ("AAACGT" :: DNASequence)


spec :: Spec
spec =
  describe "Tests for DNA" $ do
--    modifyMaxSuccess (const 10000) $
--        it "DNASequence is Semigroup" $ property $ \a b c ->
--          a <> (b <> c) == ((a <> b) <> c :: DNASequence)
    it "Concat DNASequence with overlapping" $
        testOverlappingDNASequence

