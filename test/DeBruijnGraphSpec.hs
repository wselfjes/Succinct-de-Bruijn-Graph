module DeBruijnGraphSpec where

import           Test.Hspec
import           Types.AssemblyGraphs
import           Types.DNA

dnaSequences = map DNASequence ["TT", "TC", "CG", "GG", "GA", "AA", "AG"]

deBruijnGraph = fromDNASequences 2 dnaSequences

testBuildSimpleGraph :: IO ()
testBuildSimpleGraph = assembledSequence `shouldBe` DNASequence "TTCGAAGG"
  where
    assembledSequence = assemblyDeBruijn deBruijnGraph

testSuccessor :: IO ()
testSuccessor = successors `shouldBe` map DNASequence ["TC", "TT"]
  where
    successors =
      map (numberToSequence 2) $
      successorEdges (bitArr deBruijnGraph) (sequenceToNumber (DNASequence "T"))
    bitArr (DeBruijnGraph _ b _ _) = b

testSequenceToNumber :: IO ()
testSequenceToNumber = num `shouldBe` 10
  where
    num = sequenceToNumber (DNASequence "GG")

testSelect :: IO ()
testSelect = s `shouldBe` 0
  where
    s = select (bitArr deBruijnGraph) 1
    bitArr (DeBruijnGraph _ b _ _) = b

testRank :: IO ()
testRank = s `shouldBe` 7
  where
    s = rank (bitArr deBruijnGraph) 40
    bitArr (DeBruijnGraph _ b _ _) = b

testSelectStartEdge :: IO ()
testSelectStartEdge = numberToSequence 2 e `shouldBe` DNASequence "TC"
  where
    e = selectStartEdge deBruijnGraph

testToNode :: IO ()
testToNode = numberToSequence 1 n `shouldBe` DNASequence "C"
  where
    n = getToNode $ DNASequence "TC"

testEulerBackTracking :: IO ()
testEulerBackTracking = s `shouldBe` map DNASequence ["TT", "TC"]
  where
    s =
      eulerBackTracking
        graph
        ((sequenceToNumber . DNASequence) "TT")
        []
        [DNASequence "TC"]
    graph = emptyDeBruijn 2

testEulerPath :: IO ()
testEulerPath = s `shouldBe` map DNASequence ["TT"]
  where
    s = eulerPath graph startEdge [] []
    startEdge = (sequenceToNumber . DNASequence) "TT"
    graph = fromDNASequences 2 [DNASequence "TT"]

testCheckEdge :: IO ()
testCheckEdge = g `shouldBe` emptyDeBruijn 2
  where
    g = graph /// DNASequence "TT"
    graph = fromDNASequences 2 [DNASequence "TT"]

spec :: Spec
spec =
  describe "Tests for de Bruijn Graph" $ do
    it "Sequence to number" testSequenceToNumber
    it "Select" testSelect
    it "Rank" testRank
    it "Successor" testSuccessor
    it "Select Start Edge" testSelectStartEdge
    it "Check edge" testCheckEdge
    it "Euler Back Tracking" testEulerBackTracking
    it "Euler Path" testEulerPath
    it "Simple cyclic genome" testBuildSimpleGraph
