{-# LANGUAGE OverloadedStrings #-}
module Data.Graph.DeBruijnGraphSpec where

import           Test.Hspec
import           Data.Graph.DeBruijnGraph
import           Data.Sequence.DNA
import           Data.BitArrays.VectorBitArray 
import           Data.BitArrays.BitArray
import           Data.Graph.Algorithms.EulerianWalk

dnaSequences :: [DNASequence]
dnaSequences = ["TT", "TC", "CG", "GG", "GA", "AA", "AG"]

deBruijnGraph :: DeBruijnGraph Nucleotide VectorBitArray
deBruijnGraph = preprocess (fromSequences 2 dnaSequences)

testSuccessor :: IO ()
testSuccessor = successors `shouldBe` ["TC", "TT"]
  where
    successors :: [DNASequence]
    successors =
      map (numberToSequence 2) $
      successorEdges (bitArr deBruijnGraph) (sequenceToNumber ("T" :: DNASequence))

testSelect :: IO ()
testSelect = s `shouldBe` 2
  where
    s = select (bitArr deBruijnGraph) 2

testRank :: IO ()
testRank = s `shouldBe` 7
  where
    s = rank (bitArr deBruijnGraph) 40

testCheckEdge :: IO ()
testCheckEdge = g `shouldBe` emptyDeBruijn 2
  where
    g = graph /// "TT"
    graph = fromSequences 2 (["TT"] :: [DNASequence]) :: DeBruijnGraph Nucleotide VectorBitArray

spec :: Spec
spec =
  describe "Tests for de Bruijn Graph" $ do
    it "Select" testSelect
    it "Rank" testRank
    it "Successor" testSuccessor
    it "Check edge" testCheckEdge
