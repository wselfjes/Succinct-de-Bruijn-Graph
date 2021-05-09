{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Plotting.ColoredDeBruijnGraph where

import qualified Data.GraphViz                     as G
import qualified Data.GraphViz.Attributes.Complete as G
import qualified Data.Text.Lazy                    as TL
import qualified Data.Text.Lazy.IO                 as TL
import           GHC.TypeLits

import           Data.RankSelectArray.Class
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
  => DeBruijnGraph n (Letter "ACGT")
  -> G.Color
  -> ([NodeGraph String], [EdgeGraph String])
toNodeEdgeList deBruijnGraph color = (nodes', edgesLabel)
  where
    edges' = toMultiplicityList deBruijnGraph
    edgesLabel = foldr 
                 (\(edge, r) result 
                    -> replicate r (show . head . edgeNodes $ edge, show . last . edgeNodes $ edge, RegularEdge color) ++ result) 
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
    let (vs, es) = toNodeEdgeList deBruijnGraph (G.RGB 0 0 0)
    let dotGraph = G.graphElemsToDot deBruijnGraphParams vs es :: G.DotGraph String
    -- 3. Render it into .dot text
    let dotText = G.printDotGraph dotGraph :: TL.Text
    -- 4. Write the contents to a file
    TL.writeFile "data/deBruijnGraph.dot" dotText
  

drawGraphs :: ColoredDeBruijnGraph n a -> IO ()
drawGraphs coloredDeBruijnGraph = putStrLn "Draw colored graph"
