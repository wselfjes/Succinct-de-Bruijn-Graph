{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Plotting.ColoredDeBruijnGraph where

import qualified Data.GraphViz                     as G
import qualified Data.GraphViz.Attributes.Complete as G
import qualified Data.Text.Lazy                    as TL
import qualified Data.Text.Lazy.IO                 as TL
import           GHC.TypeLits

import           Data.DNA.Assembly
import           Data.DNA.ColoredDeBruijnGraph
import           Data.Enum.Letter


data NLabel = RegularNode  
  deriving (Eq)
newtype ELabel = RegularEdge {color :: G.Color}
  deriving (Eq)
type NodeGraph a = (a, NLabel)
type EdgeGraph a = (a, a, ELabel)


toNodeEdgeList
  :: (KnownNat n, KnownNat (n+1))
  => G.Color
  -> DeBruijnGraph n (Letter "ACGT")
  -> ([NodeGraph String], [EdgeGraph String])
toNodeEdgeList c deBruijnGraph = (nodes', edgesLabel)
  where
    edges' = toMultiplicityList deBruijnGraph
    edgesLabel = foldr 
                 (\(edge, r) result 
                    -> replicate r (show . head . edgeNodes $ edge, show . last . edgeNodes $ edge, RegularEdge c) ++ result) 
                 [] edges' 
    nodes' = zip (map show (nodes deBruijnGraph)) (repeat RegularNode)

deBruijnGraphParams :: G.GraphvizParams String NLabel ELabel () NLabel
deBruijnGraphParams = G.defaultParams {
  G.fmtNode = const $ colorAttribute $ G.RGB 0 0 0,
  G.fmtEdge = fmtEdgeFun
      }
  where
    colorAttribute color = [ G.Color $ G.toColorList [ color ] ]
    fmtEdgeFun (_, _, RegularEdge c) = colorAttribute c

drawGraph :: (KnownNat n, KnownNat (n+1)) => DeBruijnGraph n (Letter "ACGT") -> IO ()
drawGraph deBruijnGraph = do
    let (vs, es) = toNodeEdgeList (G.RGB 0 0 0) deBruijnGraph
    let dotGraph = G.graphElemsToDot deBruijnGraphParams vs es :: G.DotGraph String
    -- 3. Render it into .dot text
    let dotText = G.printDotGraph dotGraph :: TL.Text
    -- 4. Write the contents to a file
    TL.writeFile "data/deBruijnGraph.dot" dotText

toNodeEdgeLists 
  :: (KnownNat n, KnownNat (n+1))
  => [G.Color]
  -> ColoredDeBruijnGraph n (Letter "ACGT")
  -> ([NodeGraph String], [EdgeGraph String])
toNodeEdgeLists colors coloredDeBruijnGraph = (nodes', edgesLabel)
  where
    edges' = toMultiplicityLists coloredDeBruijnGraph
    edgesLabel = concatMap foldrFun (zip colors edges')
    foldrFun (c, edges) = foldr (\edge result -> foldFun (c, edge) ++ result) [] edges
    foldFun (c, (edge, r)) = replicate r (edgeToNodesString head edge, edgeToNodesString last edge, RegularEdge c)
    edgeToNodesString side = show . side . edgeNodes
    nodes' = zip (map show (allNodes coloredDeBruijnGraph)) (repeat RegularNode)
  

drawGraphs :: (KnownNat n, KnownNat (n+1)) => ColoredDeBruijnGraph n (Letter "ACGT") -> IO ()
drawGraphs coloredDeBruijnGraph = do
    let (vs, es) = toNodeEdgeLists (repeat (G.RGB 0 0 0)) coloredDeBruijnGraph
    let dotGraph = G.graphElemsToDot deBruijnGraphParams vs es :: G.DotGraph String
    -- 3. Render it into .dot text
    let dotText = G.printDotGraph dotGraph :: TL.Text
    -- 4. Write the contents to a file
    TL.writeFile "data/deBruijnGraph.dot" dotText
