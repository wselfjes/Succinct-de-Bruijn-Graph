{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Sequence.DNASpec where

import           Test.Hspec
import           Test.QuickCheck
import           Data.Sequence.DNA


instance Arbitrary Nucleotide where
  arbitrary = elements allNucleotides

instance (Arbitrary a) => Arbitrary (Sequence a) where
  arbitrary = Sequence <$> arbitrary

testOverlappingDNASequence :: IO ()
testOverlappingDNASequence =(("AAAC" :: DNASequence) `mergeDNASequence` ("ACGT" :: DNASequence))
                             `shouldBe` ("AAACGT" :: DNASequence)

testSequenceToNumber :: IO ()
testSequenceToNumber = num `shouldBe` 10
  where
    num = sequenceToNumber ("GG" :: DNASequence)

testToNode :: IO ()
testToNode = numberToSequence 1 n `shouldBe` ("C" :: DNASequence)
  where
    n = getToNode "TC"

spec :: Spec
spec =
  describe "Tests for DNA" $ do
    it "Concat DNASequence with overlapping" $
        testOverlappingDNASequence
    it "Test sequence to number" $
        testSequenceToNumber
    it "Sequence to number" $
        testSequenceToNumber

