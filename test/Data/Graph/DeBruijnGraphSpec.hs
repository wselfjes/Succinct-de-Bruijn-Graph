{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Graph.DeBruijnGraphSpec where

import           Data.RankSelectArray.VectorBitArray
import           Data.Graph.Algorithms.EulerianWalk
import           Data.Graph.DeBruijnGraph
import           Data.Sequence.DNA
import           Test.Hspec

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

testCheckEdge :: IO ()
testCheckEdge = g `shouldBe` emptyDeBruijn 2
  where
    g = graph /// "TT"
    graph = fromSequences 2 (["TT"] :: [DNASequence]) :: DeBruijnGraph Nucleotide VectorBitArray

spec :: Spec
spec =
  describe "Tests for de Bruijn Graph" $ do
    it "Successor" testSuccessor
    it "Check edge" testCheckEdge
