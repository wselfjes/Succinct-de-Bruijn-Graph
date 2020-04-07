{-# LANGUAGE OverloadedStrings #-}
module Types.DeBruijnGraphSpec where

import           Test.Hspec
import           Types.AssemblyGraphs
import           Types.DNA

dnaSequences :: [DNASequence]
dnaSequences = ["TT", "TC", "CG", "GG", "GA", "AA", "AG"]

deBruijnGraph :: DeBruijnGraph Nucleotide
deBruijnGraph = fromSequences 2 dnaSequences

testBuildSimpleGraph :: IO ()
testBuildSimpleGraph =
  assembledSequence `shouldBe` "TTCGAAGG"
  where
    assembledSequence = assemblyDeBruijn deBruijnGraph

testSuccessor :: IO ()
testSuccessor = successors `shouldBe` ["TC", "TT"]
  where
    successors :: [DNASequence]
    successors =
      map (numberToSequence 2) $
      successorEdges (bitArr deBruijnGraph) (sequenceToNumber ("T" :: DNASequence))

testSequenceToNumber :: IO ()
testSequenceToNumber = num `shouldBe` 10
  where
    num = sequenceToNumber ("GG" :: DNASequence)

testSelect :: IO ()
testSelect = s `shouldBe` 2
  where
    s = select (bitArr deBruijnGraph) 2

testRank :: IO ()
testRank = s `shouldBe` 7
  where
    s = rank (bitArr deBruijnGraph) 40

testSelectStartEdge :: IO ()
testSelectStartEdge = numberToSequence 2 e `shouldBe` ("TC" :: DNASequence)
  where
    e = selectStartEdge deBruijnGraph

testSelectNodes :: IO ()
testSelectNodes = nodes `shouldBe` (3, 2)
  where
    nodes = selectNodes (multipliedVec deBruijnGraph) (graphBase deBruijnGraph)

testToNode :: IO ()
testToNode = numberToSequence 1 n `shouldBe` ("C" :: DNASequence)
  where
    n = getToNode "TC"

testEulerBackTracking :: IO ()
testEulerBackTracking = s `shouldBe` ["TT", "TC"]
  where
    s =
      eulerBackTracking
        graph
        (Just (sequenceToNumber ("TT" :: DNASequence)))
        []
        (["TC"] :: [DNASequence])
    graph = emptyDeBruijn 2

testEulerPath :: IO ()
testEulerPath = s `shouldBe` ["TT"]
  where
    s = eulerPath graph startEdge [] []
    startEdge = sequenceToNumber ("TT" :: DNASequence)
    graph = fromSequences 2 (["TT"] :: [DNASequence])

testCheckEdge :: IO ()
testCheckEdge = g `shouldBe` emptyDeBruijn 2
  where
    g = graph /// "TT"
    graph = fromSequences 2 (["TT"] :: [DNASequence])

spec :: Spec
spec =
  describe "Tests for de Bruijn Graph" $ do
    it "Sequence to number" testSequenceToNumber
    it "Select" testSelect
    it "Rank" testRank
    it "Successor" testSuccessor
    it "Select Pivoting Nodes" testSelectNodes
    it "Select Start Edge" testSelectStartEdge
    it "Check edge" testCheckEdge
    it "Euler Back Tracking" testEulerBackTracking
    it "Euler Path" testEulerPath
    it "Simple cyclic genome" testBuildSimpleGraph
