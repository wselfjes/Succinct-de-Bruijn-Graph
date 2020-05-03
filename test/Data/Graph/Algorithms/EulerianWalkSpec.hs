{-# LANGUAGE OverloadedStrings #-}
module Data.Graph.Algorithms.EulerianWalkSpec where

import           Data.BitArrays.BitArray
import           Data.BitArrays.VectorBitArray
import           Data.Graph.Algorithms.EulerianWalk
import           Data.Graph.DeBruijnGraph
import           Data.Sequence.DNA
import           Test.Hspec

testBuildSimpleGraph :: IO ()
testBuildSimpleGraph =
  assembledSequence `shouldBe` "ATGGCGTGCA"
  where
    assembledSequence = assemblyDeBruijnUsingEulerianWalk deBruijnGraph'
    deBruijnGraph' = preprocess (fromSequences 3 dnaSequences') :: DeBruijnGraph Nucleotide VectorBitArray
    dnaSequences' = ["ATGGCGT", "CGCGTGC", "CGTGCAA", "TGCAATG", "CAATGGC"] :: [DNASequence]

testSelectNodes :: IO ()
testSelectNodes = nodes `shouldBe` (3, 2)
  where
    nodes = (0, 0)-- selectNodes (multipliedVec deBruijnGraph) (graphBase deBruijnGraph)

testSelectStartEdge :: IO ()
testSelectStartEdge = numberToSequence 2 e `shouldBe` ("TC" :: DNASequence)
  where
    e = 0 --selectStartEdge deBruijnGraph

testEulerBackTracking :: IO ()
testEulerBackTracking = s `shouldBe` ["TT", "TC"]
  where
    s =
      eulerBackTracking
        graph
        (Just (sequenceToNumber ("TT" :: DNASequence)))
        []
        (["TC"] :: [DNASequence])
    graph = emptyDeBruijn 2 :: DeBruijnGraph Nucleotide VectorBitArray

testEulerPath :: IO ()
testEulerPath = s `shouldBe` ["TT"]
  where
    s = eulerPath graph startEdge [] []
    startEdge = sequenceToNumber ("TT" :: DNASequence)
    graph = fromSequences 2 (["TT"] :: [DNASequence]) :: DeBruijnGraph Nucleotide VectorBitArray

spec :: Spec
spec =
  describe "Tests for Eulerian Walk" $ do
    it "Select Pivoting Nodes" testSelectNodes
    it "Select Start Edge" testSelectStartEdge
    it "Euler Back Tracking" testEulerBackTracking
    it "Euler Path" testEulerPath
    it "Simple cyclic genome" testBuildSimpleGraph
