module Types.DeBruijnGraphSpec where

import           Test.Hspec
import           Types.AssemblyGraphs
import           Types.DNA

dnaSequences :: [DNASequence]
dnaSequences =
  map DNASequence [[T, T], [T, C], [C, G], [G, G], [G, A], [A, A], [A, G]]

deBruijnGraph :: DeBruijnGraph
deBruijnGraph = fromDNASequences 2 dnaSequences

testBuildSimpleGraph :: IO ()
testBuildSimpleGraph =
  assembledSequence `shouldBe` DNASequence [T, T, C, G, A, A, G, G]
  where
    assembledSequence = assemblyDeBruijn deBruijnGraph

testSuccessor :: IO ()
testSuccessor = successors `shouldBe` map DNASequence [[T, C], [T, T]]
  where
    successors =
      map (numberToSequence 2) $
      successorEdges (bitArr deBruijnGraph) (sequenceToNumber (DNASequence [T]))

testSequenceToNumber :: IO ()
testSequenceToNumber = num `shouldBe` 10
  where
    num = sequenceToNumber (DNASequence [G, G])

testSelect :: IO ()
testSelect = s `shouldBe` 2
  where
    s = select (bitArr deBruijnGraph) 2

testRank :: IO ()
testRank = s `shouldBe` 7
  where
    s = rank (bitArr deBruijnGraph) 40

testSelectStartEdge :: IO ()
testSelectStartEdge = numberToSequence 2 e `shouldBe` DNASequence [T, C]
  where
    e = selectStartEdge deBruijnGraph

testSelectNodes :: IO ()
testSelectNodes = nodes `shouldBe` (3, 2)
  where
    nodes = selectNodes (multipliedVec deBruijnGraph) (graphBase deBruijnGraph)

testToNode :: IO ()
testToNode = numberToSequence 1 n `shouldBe` DNASequence [C]
  where
    n = getToNode $ DNASequence [T, C]

testEulerBackTracking :: IO ()
testEulerBackTracking = s `shouldBe` map DNASequence [[T, T], [T, C]]
  where
    s =
      eulerBackTracking
        graph
        (Just ((sequenceToNumber . DNASequence) [T, T]))
        []
        [DNASequence [T, C]]
    graph = emptyDeBruijn 2

testEulerPath :: IO ()
testEulerPath = s `shouldBe` [DNASequence [T, T]]
  where
    s = eulerPath graph startEdge [] []
    startEdge = (sequenceToNumber . DNASequence) [T, T]
    graph = fromDNASequences 2 [DNASequence [T, T]]

testCheckEdge :: IO ()
testCheckEdge = g `shouldBe` emptyDeBruijn 2
  where
    g = graph /// DNASequence [T, T]
    graph = fromDNASequences 2 [DNASequence [T, T]]

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
