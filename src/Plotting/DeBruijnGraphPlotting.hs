module Plotting.DeBruijnGraphPlotting where

import           Data.Graph.DeBruijnGraph
import           Data.Sequence.DNA
import           Data.BitArrays.BitArray
import qualified Data.GraphViz as G
import qualified Data.GraphViz.Attributes.Complete as G
import           Data.List.Unique 
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL

data NLabel = RegularNode
  deriving (Eq)
data ELabel = RegularEdge
  deriving (Eq)
type NodeGraph a = (a, NLabel)
type EdgeGraph a = (a, a, ELabel)


toNodeEdgeList 
  :: (BitArray b)
  => DeBruijnGraph Nucleotide b
  -> ([NodeGraph String], [EdgeGraph String])
toNodeEdgeList deBruijnGrpah = (allNodes, allEdges)
  where
    multiplicityList = filter ((>0) . snd) (toMultiplicityList deBruijnGrpah)
    allEdges = concatMap getEdge multiplicityList
    getEdge :: (DNASequence, Int) -> [(EdgeGraph String)]
    getEdge (sequenceEdge, num) = take num $ repeat ((show . toSequence) fromNode, (show . toSequence) toNode, RegularEdge)
      where
        toNode = getToNode sequenceEdge
        fromNode = getFromNode sequenceEdge
        toSequence :: Int -> DNASequence
        toSequence = numberToSequence (graphBase deBruijnGrpah)
    getNode (node1, node2, _) = [(node1, RegularNode), (node2, RegularNode)]
    allNodes = uniq (concatMap getNode allEdges)


deBruijnGraphParams :: G.GraphvizParams String NLabel ELabel () NLabel
deBruijnGraphParams = G.defaultParams {
  G.fmtNode = const $ colorAttribute $ G.RGB 0 0 0,
  G.fmtEdge = const $ colorAttribute $ G.RGB 0 0 0
      }  
  where
    colorAttribute color = [ G.Color $ G.toColorList [ color ] ]
    
drawGraph 
  :: (BitArray b)
  => DeBruijnGraph Nucleotide b 
  -> IO ()
drawGraph deBruijnGraph = do
  let (vs, es) = toNodeEdgeList deBruijnGraph
  let dotGraph = G.graphElemsToDot deBruijnGraphParams vs es :: G.DotGraph String
  -- 3. Render it into .dot text
  let dotText = G.printDotGraph dotGraph :: TL.Text
  -- 4. Write the contents to a file
  TL.writeFile "data/deBruijnGraph.dot" dotText
